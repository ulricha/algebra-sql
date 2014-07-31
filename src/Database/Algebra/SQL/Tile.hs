{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Algebra.SQL.Tile
    ( TileTree (TileNode, ReferenceLeaf)
    , TileChildren
    , ExternalReference
    , InternalReference
    , DependencyList
    , TransformResult
    , transform
    , TADag
    ) where

-- TODO maybe split this file into the tile definition
--      and the transform things.
-- TODO embed closing tiles as subqueries (are there any sub queries which are
-- correlated?)? (reader?)
-- TODO isMultiReferenced special case: check for same parent !!

import           Control.Arrow                    (second)
import           Control.Monad                    (liftM, when)
import           Control.Monad.Trans.RWS.Strict
import qualified Data.DList                       as DL (DList, singleton)
import qualified Data.IntMap                      as IntMap
import           Data.Maybe

import qualified Database.Algebra.Dag             as D
import qualified Database.Algebra.Dag.Common      as C
import           Database.Algebra.Impossible
import qualified Database.Algebra.Table.Lang      as A

import qualified Database.Algebra.SQL.Query       as Q
import           Database.Algebra.SQL.Termination
import Database.Algebra.SQL.Query.Util

-- | A tile internal reference type.
type InternalReference = Q.ReferenceType

-- | The type used to reference table expressions outside of a tile.
type ExternalReference = Int

-- | Aliased tile children, where the first part is the alias used within the
-- 'Q.SelectStmt'.
type TileChildren = [(InternalReference, TileTree)]

-- | Defines the tile tree structure.
data TileTree = -- | A tile: The first argument determines which features the
                -- 'Q.SelectStmt' uses.
                TileNode FeatureSet Q.SelectStmt TileChildren
                -- | A reference pointing to another TileTree: The second
                -- argument specifies the columns of the referenced table
                -- expression.
              | ReferenceLeaf ExternalReference [String]

-- | Table algebra DAGs
type TADag = D.AlgebraDag A.TableAlgebra

-- | Association list (where dependencies should be ordered topologically).
type DependencyList = DL.DList (ExternalReference, TileTree)


-- | A combination of types which need to be modified state wise while
-- transforming:
--     * The processed nodes with multiple parents.
--
--     * The current state of the table id generator.
--
--     * The current state of the variable id generator.
--
data TransformState = TS
                    { multiParentNodes :: IntMap.IntMap ( ExternalReference
                                                        , [String]
                                                        )
                    , tableIdGen       :: ExternalReference
                    , aliasIdGen       :: Int
                    , varIdGen         :: InternalReference
                    }

-- | The initial state.
sInitial :: TransformState
sInitial = TS IntMap.empty 0 0 0

-- | Adds a new binding to the state.
sAddBinding :: C.AlgNode          -- ^ The key as a node with multiple parents.
            -> ( ExternalReference
               , [String]
               )                  -- ^ Name of the reference and its columns.
            -> TransformState
            -> TransformState
sAddBinding node t st =
    st { multiParentNodes = IntMap.insert node t $ multiParentNodes st}

-- | Tries to look up a binding for a node.
sLookupBinding :: C.AlgNode
               -> TransformState
               -> Maybe (ExternalReference, [String])
sLookupBinding n = IntMap.lookup n . multiParentNodes

-- | The transform monad is used for transforming from DAGs into the tile plan. It
-- contains:
--
--     * A reader for the DAG (since we only read from it)
--
--     * A writer for outputting the dependencies
--
--     * A state for generating fresh names and maintain the mapping of nodes
--
type TransformMonad = RWS TADag DependencyList TransformState

-- | A table expression id generator using the state within the
-- 'TransformMonad'.
generateTableId :: TransformMonad ExternalReference
generateTableId = do
    st <- get

    let tid = tableIdGen st

    put $ st { tableIdGen = succ tid }

    return tid

generateAliasName :: TransformMonad String
generateAliasName = do
    st <- get

    let aid = aliasIdGen st

    put $ st { aliasIdGen = succ aid }

    return $ 'a' : show aid

-- | A variable identifier generator.
generateVariableId :: TransformMonad InternalReference
generateVariableId = do
    st <- get

    let vid = varIdGen st

    put $ st { varIdGen = succ vid }

    return vid

-- | Unpack values (or run computation).
runTransformMonad :: TransformMonad a
                  -> TADag                      -- ^ The used DAG.
                  -> TransformState             -- ^ The inital state.
                  -> (a, DependencyList)
runTransformMonad = evalRWS

-- | Check if node has more than one parent.
isMultiReferenced :: C.AlgNode
                  -> TADag
                  -> Bool
isMultiReferenced n dag = case D.parents n dag of
    -- Has at least 2 parents.
    _:(_:_) -> True
    _       -> False

-- | Get the column schema of a 'TileNode'.
getSchemaTileTree :: TileTree -> [String]
getSchemaTileTree (ReferenceLeaf _ s) = s
getSchemaTileTree (TileNode _ body _) = getSchemaSelectStmt body

-- | Get the column schema of a 'Q.SelectStmt'.
getSchemaSelectStmt :: Q.SelectStmt -> [String]
getSchemaSelectStmt s = map Q.sName $ Q.selectClause s

-- | The result of the 'transform' function.
type TransformResult = ([TileTree], DependencyList)

-- | Transform a 'TADag', while swapping out repeatedly used sub expressions
-- (nodes with more than one parent).
-- A 'TADag' can have multiple root nodes, and therefore the function returns a
-- list of root tiles and their dependencies.
transform :: TADag -> TransformResult
transform dag = runTransformMonad result dag sInitial
  where
    rootNodes = D.rootNodes dag
    result    = mapM transformNode rootNodes

-- | This function basically checks for already referenced nodes with more than
-- one parent, returning a reference to already computed 'TileTree's.
transformNode :: C.AlgNode -> TransformMonad TileTree
transformNode n = do

    op <- asks $ D.operator n

    -- allowBranch indicates whether multi reference nodes shall be split
    -- for this operator, resulting in multiple equal branches. (Treeify)
    let (allowBranch, transformOp) = case op of
                                   -- Ignore branching for nullary operators.
            (C.NullaryOp nop)   -> (False, transformNullaryOp nop)
            (C.UnOp uop c)      -> (True, transformUnOp uop c)
            (C.BinOp bop c0 c1) -> (True, transformBinOp bop c0 c1)
            (C.TerOp () _ _ _)  -> $impossible

    multiRef <- asks $ isMultiReferenced n

    if allowBranch && multiRef
    then do
        -- Lookup whether there exists a binding for the node in the current
        -- state.
        possibleBinding <- gets $ sLookupBinding n

        case possibleBinding of
            -- If so, just return it.
            Just (b, s) -> return $ ReferenceLeaf b s
            -- Otherwise add it.
            Nothing     -> do

                resultingTile <- transformOp

                -- Generate a name for the sub tree.
                tableId <- generateTableId

                -- Add the tree to the writer.
                tell $ DL.singleton (tableId, resultingTile)

                let schema = getSchemaTileTree resultingTile

                -- Add binding for this node (to prevent recalculation).
                modify $ sAddBinding n (tableId, schema)

                return $ ReferenceLeaf tableId schema
    else transformOp

transformNullaryOp :: A.NullOp -> TransformMonad TileTree
transformNullaryOp (A.LitTable [] schema) = do
    alias <- generateAliasName

    let sFun n   = Q.SCAlias (Q.EEBase $ mkPCol alias n) n
        sClause  = map (sFun . fst) schema
        castedNull ty = Q.CEBase $ Q.VECast (Q.CEBase $ Q.VEValue Q.VNull)
                                            (translateATy ty)
        fLiteral = Q.FPAlias (Q.FESubQuery $ Q.VQLiteral [map (castedNull . snd) schema])
                             alias
                             $ Just $ map fst schema

    return $ TileNode
             (projectF <> filterF <> tableF)
             emptySelectStmt
             { Q.selectClause = sClause
             , Q.fromClause = [fLiteral]
             , Q.whereClause = [Q.CEBase . Q.VEValue $ Q.VBoolean False]
             }
             []
transformNullaryOp (A.LitTable tuples schema) = do
    alias <- generateAliasName

    let sFun n   = Q.SCAlias (Q.EEBase $ mkPCol alias n) n
        sClause  = map (sFun . fst) schema
        fLiteral = Q.FPAlias (Q.FESubQuery $ Q.VQLiteral $ map tMap tuples)
                             alias
                             $ Just $ map fst schema

    return $ TileNode
             (projectF <> tableF)
             emptySelectStmt
             { Q.selectClause = sClause
             , Q.fromClause = [fLiteral]
             }
             []
  where
    tMap = map $ Q.CEBase . Q.VEValue . translateAVal

transformNullaryOp (A.TableRef (name, info, _))   = do
    alias <- generateAliasName

    let f (n, _) = Q.SCAlias (Q.EEBase $ mkPCol alias n) n
        body     =
            emptySelectStmt
            { -- Map the columns of the table reference to the given
              -- column names.
              Q.selectClause = map f info
            , Q.fromClause =
                    [ Q.FPAlias (Q.FETableReference name)
                                alias
                                -- Map to old column name.
                                $ Just $ map fst info
                    ]
            }

    return $ TileNode (projectF <> tableF) body []


-- | Abstraction for rank operators.
transformUnOpRank :: -- ExtendedExpr constructor.
                     ([Q.WindowOrderExpr] -> Q.ExtendedExpr)
                  -> (String, [A.SortAttr])
                  -> C.AlgNode
                  -> TransformMonad TileTree
transformUnOpRank rankConstructor (name, sortList) =
    attachColFunUnOp colFun $ projectF <> windowFunctionF
  where
    colFun sClause = Q.SCAlias
                     ( rankConstructor $ asWindowOrderExprList
                                         sClause
                                         sortList
                     )
                     name

transformUnOp :: A.UnOp -> C.AlgNode -> TransformMonad TileTree
transformUnOp (A.Serialize (mDescr, pos, payloadCols)) c = do

    (childFeatures, select, children) <-
        transformTerminated c opFeatures

    let sClause              = Q.selectClause select
        inline               = inlineEE sClause
        project (col, alias) = Q.SCAlias (inline col) alias
        itemi i              = "item" ++ show i
        payloadProjs         =
            zipWith (\(A.PayloadCol col) i -> Q.SCAlias (inline col) (itemi i))
                    payloadCols
                    [1..]

    return $ TileNode
             (childFeatures <> opFeatures)
             select
             { Q.selectClause = map project (descrProjList ++ posProjList)
                                ++ payloadProjs
             , -- Order by optional columns. Remove constant column expressions,
               -- since SQL99 defines different semantics.
               Q.orderByClause =
                   map (`Q.OE` Q.Ascending)
                       $ descrColAdder . filter affectsSortOrderEE
                         $ map inline posOrderList
             }
             children
  where
    opFeatures                     = projectF <> orderingF
    (descrColAdder, descrProjList) = case mDescr of
        Nothing               -> (id, [])
        -- Project and sort. Since descr gets added as new alias we can use it
        -- in the ORDER BY clause.
        Just (A.DescrCol col) -> ( (:) $ Q.EEBase $ mkCol "descr"
                                 , [(col, "descr")]
                                 )

    (posOrderList, posProjList)     = case pos of
        A.NoPos       -> ([], [])
        -- Sort and project.
        A.AbsPos col  -> ([col], [(col, "pos")])
        -- Sort but do not project. It is not necessary because
        -- relative positions are not needed to reconstruct nested
        -- results.
        A.RelPos cols -> (cols, [])


transformUnOp (A.RowNum (name, sortList, optPart)) c =
    attachColFunUnOp colFun
                     (projectF <> windowFunctionF)
                     c
  where
    colFun sClause = Q.SCAlias rowNumExpr name
        where
          rowNumExpr = Q.EERowNum (liftM (inlineAE sClause) optPart)
                                  $ asWindowOrderExprList sClause sortList

transformUnOp (A.RowRank inf) c = transformUnOpRank Q.EEDenseRank inf c
transformUnOp (A.Rank inf) c = transformUnOpRank Q.EERank inf c
transformUnOp (A.Project projList) c = do
    
    (childFeatures, select, children) <-
        transformTerminated c opFeatures

    let sClause  = Q.selectClause select
        -- Inlining is obligatory here, since we possibly eliminate referenced
        -- columns. ('translateExpr' inlines columns.)
        translateAlias :: (A.AttrName, A.Expr) -> Q.SelectColumn
        translateAlias (col, expr) = Q.SCAlias translatedExpr col
          where
            translatedExpr = translateExprEE (Just sClause) expr

    return $ TileNode
             (opFeatures <> childFeatures)
             -- Replace the select clause with the projection list.
             select { Q.selectClause = map translateAlias projList }
             -- But use the old children.
             children
  where
    opFeatures = projectF


transformUnOp (A.Select expr) c = do

    (childFeatures, select, children) <-
        transformTerminated c opFeatures
    
    return $ TileNode
             (opFeatures <> childFeatures)
             ( appendToWhere ( translateExprCE
                               (Just $ Q.selectClause select)
                               expr
                             )
               select
             )
             children
  where
    opFeatures = filterF

transformUnOp (A.Distinct ()) c = do

    (childFeatures, select, children) <-
        transformTerminated c opFeatures

    -- Keep everything but set distinct.
    return $ TileNode (opFeatures <> childFeatures)
                      select { Q.distinct = True }
                      children
  where
    opFeatures = dupElimF

transformUnOp (A.Aggr (aggrs, partExprMapping)) c = do

    (childFeatures, select, children) <-
        transformTerminated c opFeatures
    
    let sClause           = Q.selectClause select
        translateE        = translateExprCE $ Just sClause
        maybeTranslateE   = liftM translateE
        -- Inlining here is obligatory, since we could eliminate referenced
        -- columns. (This is similar to projection.)
        aggrToEE (a, n)   = 
            Q.SCAlias ( let (fun, optExpr) = translateAggrType a
                        in Q.EEAggrExpr
                           $ Q.AEAggregate (maybeTranslateE optExpr)
                                           fun
                      )
                      n

        partColumnExprs   = map (second translateE) partExprMapping
        partExtendedExprs = map (second $ translateExprEE $ Just sClause)
                            partExprMapping


        wrapSCAlias (name, extendedExpr)
                        =
            Q.SCAlias extendedExpr name

    return $ TileNode
             (childFeatures <> opFeatures)
             select
             { Q.selectClause =
                   map wrapSCAlias partExtendedExprs ++ map aggrToEE aggrs
             , -- Since SQL treats numbers in the group by clause as column
               -- indices, filter them out. (They do not change the semantics
               -- anyway.)
               Q.groupByClause =
                   filter affectsSortOrderCE $ map snd partColumnExprs
             }
             children
  where
    opFeatures = projectF <> aggrAndGroupingF

-- | Generates a new 'TileTree' by attaching a column, generated by a function
-- taking the select clause.
attachColFunUnOp :: ([Q.SelectColumn] -> Q.SelectColumn)
                 -> FeatureSet
                 -> C.AlgNode
                 -> TransformMonad TileTree
attachColFunUnOp colFun opFeatures c = do

    (childFeatures, select, children) <-
        transformTerminated c opFeatures

    let sClause = Q.selectClause select
    return $ TileNode
             (opFeatures <> childFeatures)
             -- Attach a column to the select clause generated by the given
             -- function.
             select { Q.selectClause = colFun sClause : sClause }
             children

-- | Abstracts over binary set operation operators.
transformBinSetOp :: Q.SetOperation
                  -> C.AlgNode
                  -> C.AlgNode
                  -> TransformMonad TileTree
transformBinSetOp setOp c0 c1 = do

    -- Use one tile to get the schema information.
    (_, select0, children0) <- transformTerminated c0 noneF
    (_, select1, children1) <- transformTerminated c1 noneF

    alias <- generateAliasName

    -- Take the schema of the first one, but could also be from the second one,
    -- since we assume they are equal.
    let schema = getSchemaSelectStmt select0

    return $ TileNode opFeatures
                      emptySelectStmt
                      { Q.selectClause =
                            columnsFromSchema alias schema
                      , Q.fromClause =
                            [ Q.FPAlias ( Q.FESubQuery
                                          $ Q.VQBinarySetOperation
                                            (Q.VQSelect select0)
                                            (Q.VQSelect select1)
                                            setOp
                                        )
                                        alias
                                        $ Just schema
                            ]
                      }
                      $ children0 ++ children1
  where
    opFeatures = projectF <> tableF


-- | Perform a cross join between two nodes.
transformBinCrossJoin :: C.AlgNode
                      -> C.AlgNode
                      -> TransformMonad ( FeatureSet
                                        , Q.SelectStmt
                                        , TileChildren
                                        )
transformBinCrossJoin c0 c1 = do

    (childFeatures0, select0, children0) <-
        transformTerminated c0 opFeatures
    (childFeatures1, select1, children1) <-
        transformTerminated c1 opFeatures

    -- We can simply concatenate everything, because all things are prefixed and
    -- cross join is associative.
    return ( mconcat [childFeatures0, childFeatures1, opFeatures]
           , emptySelectStmt
             { Q.selectClause =
                   Q.selectClause select0 ++ Q.selectClause select1
             , Q.fromClause =
                   Q.fromClause select0 ++ Q.fromClause select1
             , Q.whereClause = Q.whereClause select0
                               ++ Q.whereClause select1
             }
           , children0 ++ children1
           )
  where
    opFeatures = projectF <> tableF <> filterF

transformBinOp :: A.BinOp
               -> C.AlgNode
               -> C.AlgNode
               -> TransformMonad TileTree
transformBinOp (A.Cross ()) c0 c1 = do
    (f, s, c) <- transformBinCrossJoin c0 c1
    return $ TileNode f s c

transformBinOp (A.EqJoin (lName, rName)) c0 c1 = do

    (childrenFeatures, select, children) <- transformBinCrossJoin c0 c1

    let sClause = Q.selectClause select
        cond    = Q.CEBase $ Q.VEBinApp Q.BFEqual (inlineCE sClause lName)
                                                  $ inlineCE sClause rName

    -- 'transformBinCrossJoin' already has the 'filterF' feature.
    return $ TileNode childrenFeatures (appendToWhere cond select) children


transformBinOp (A.ThetaJoin conditions) c0 c1  = do

    when (null conditions) $impossible

    (childrenFeatures, select, children) <- transformBinCrossJoin c0 c1

    let sClause = Q.selectClause select
        conds   = map f conditions
        f       = translateJoinCond sClause sClause

    return $ TileNode childrenFeatures
                        (appendAllToWhere conds select)
                        children

transformBinOp (A.SemiJoin cs) c0 c1          =
    transformExistsJoin cs c0 c1 id
transformBinOp (A.AntiJoin cs) c0 c1          =
    transformExistsJoin cs c0 c1 (Q.CEBase . Q.VENot)
transformBinOp (A.DisjUnion ()) c0 c1         =
    transformBinSetOp Q.SOUnionAll c0 c1
transformBinOp (A.Difference ()) c0 c1        =
    transformBinSetOp Q.SOExceptAll c0 c1

transformExistsJoin :: A.SemInfJoin
                    -> C.AlgNode 
                    -> C.AlgNode
                    -> (Q.ColumnExpr -> Q.ColumnExpr)
                    -> TransformMonad TileTree
transformExistsJoin conditions c0 c1 existsWrapF = do

    when (null conditions) $impossible

    (childFeatures0, select0, children0) <-
        transformTerminated c0 opFeatures

    -- Ignore operator features, since it will be nested and therefore
    -- terminated.
    (_, select1, children1) <- transformTerminated c1 noneF

    let newFeatures = opFeatures <> childFeatures0
        newChildren = children0 ++ children1
        ctor s      = TileNode newFeatures s newChildren
    
    case result of
        (Nothing, _)               -> do
            -- TODO in case we do not have merge conditions we can simply use
            -- the unmergeable but less nested select stmt on the right side

            let outerCond   = existsWrapF
                              . Q.CEBase
                              . Q.VEExists
                              $ Q.VQSelect innerSelect
                innerSelect = appendAllToWhere innerConds select1
                innerConds  = map f conditions
                f           = translateJoinCond (Q.selectClause select0)
                                                $ Q.selectClause select1

            return $ ctor (appendToWhere outerCond select0)
        (Just (l, r), conditions') -> do
           
            let -- Embedd the right query into the where clause of the left one.
                leftCond    =
                    existsWrapF $ Q.CEBase
                                  $ Q.VEIn (translateExprCE (Just lSClause) l)
                                           $ Q.VQSelect rightSelect
                -- Embedd all conditions in the right select, and set select
                -- clause to the right part of the equal join condition.
                rightSelect = appendAllToWhere innerConds select1
                              { Q.selectClause = [rightSCol] }
                innerConds  = map f conditions'
                f           = translateJoinCond lSClause rSClause
                rightSCol   = Q.SCAlias (translateExprEE (Just rSClause) r)
                                        "rightCond"
                lSClause    = Q.selectClause select0
                rSClause    = Q.selectClause select1

            return $ ctor (appendToWhere leftCond select0)
  where
    result                                = foldr tryIn (Nothing, []) conditions
    -- Tries to extract a join condition for usage in the IN sql construct.
    tryIn c (Just eqCols, r)              = (Just eqCols, c:r)
    tryIn c@(left, right, j) (Nothing, r) = case j of
        A.EqJ -> (Just (left, right), r)
        _     -> (Nothing, c:r)
    opFeatures                            = filterF

-- | Terminates a SQL fragment when suggested. Returns the resulting
-- 'FeatureSet' of the child, the 'Q.SelectStmt' and its children.
transformTerminated :: C.AlgNode
                    -> FeatureSet
                    -> TransformMonad (FeatureSet, Q.SelectStmt, TileChildren)
transformTerminated n topFs = do
    tile <- transformNode n
    
    case tile of
        TileNode bottomFs body children
            | topFs `terminatesOver` bottomFs -> do
                alias <- generateAliasName

                let schema = getSchemaSelectStmt body

                return ( projectF <> tableF
                       , emptySelectStmt
                         { Q.selectClause =
                               columnsFromSchema alias schema
                         , Q.fromClause =
                               [mkSubQuery body alias $ Just schema]
                         }
                       , children
                       )
            | otherwise                       -> return (bottomFs, body, children)
        ReferenceLeaf r s                     -> do
                (sel, cs) <- embedExternalReference r s
                return (projectF <> tableF, sel, cs)

-- | Embeds an external reference into a 'Q.SelectStmt'.
embedExternalReference :: ExternalReference
                       -> [String]
                       -> TransformMonad (Q.SelectStmt, TileChildren)
embedExternalReference extRef schema = do

        alias <- generateAliasName
        varId <- generateVariableId

        return ( emptySelectStmt
                   -- Use the schema to construct the select clause.
                 { Q.selectClause =
                       columnsFromSchema alias schema
                 , Q.fromClause =
                       [mkFromPartVar varId alias $ Just schema]
                 }
               , [(varId, ReferenceLeaf extRef schema)]
               )

-- | Generate a select clause with column names from a schema and a prefix.
columnsFromSchema :: String -> [String] -> [Q.SelectColumn]
columnsFromSchema p = map (asSelectColumn p)

-- | Creates 'Q.SelectColumn' which points at a prefixed column with the same
-- name.
asSelectColumn :: String
               -> String
               -> Q.SelectColumn
asSelectColumn prefix columnName =
    Q.SCAlias (Q.EEBase $ mkPCol prefix columnName) columnName

-- Translates a '[A.SortAttr]' into a '[Q.WindowOrderExpr]'. Column names will
-- be inlined as a 'Q.AggrExpr', constant ones will be discarded.
asWindowOrderExprList :: [Q.SelectColumn]
                      -> [A.SortAttr]
                      -> [Q.WindowOrderExpr]
asWindowOrderExprList sClause si =
    filter (affectsSortOrderAE . Q.woExpr)
           $ translateSortInf si (inlineAE sClause)

-- | Search the select clause for a specific column definition and return it as
-- 'Q.ColumnExpr'.
inlineCE :: [Q.SelectColumn]
         -> String
         -> Q.ColumnExpr
inlineCE sClause col =
    fromMaybe (Q.CEBase $ Q.VEColumn col Nothing)
              $ convertEEtoCE $ inlineEE sClause col


-- | Search the select clause for a specific column definition and return it as
-- 'Q.AggrExpr'.
inlineAE :: [Q.SelectColumn]
         -> String
         -> Q.AggrExpr
inlineAE sClause col =
    fromMaybe (Q.AEBase $ Q.VEColumn col Nothing)
              $ convertEEtoAE $ inlineEE sClause col

-- | Search the select clause for a specific column definition and return it as
-- 'Q.ExtendedExpr'.
inlineEE :: [Q.SelectColumn]
         -> String
         -> Q.ExtendedExpr
inlineEE sClause col =
    fromMaybe (Q.EEBase $ mkCol col) $ foldr f Nothing sClause
  where
    f (Q.SCAlias ae a) r | col == a  = return ae
                         | otherwise = r

-- | Generic base converter for the value expression template. Since types do
-- not have equal functionality, conversion can fail.
convertEEBaseTemplate :: (Q.ExtendedExpr -> Maybe a)
                      -> Q.ExtendedExprBase
                      -> Maybe (Q.ValueExprTemplate a)
convertEEBaseTemplate convertEEBaseRec eeb = case eeb of
    Q.VEValue v             -> return $ Q.VEValue v
    Q.VEColumn n p          -> return $ Q.VEColumn n p
    Q.VECast rec t          -> do
        e <- convertEEBaseRec rec
        return $ Q.VECast e t

    Q.VEBinApp f lrec rrec  -> do
        l <- convertEEBaseRec lrec
        r <- convertEEBaseRec rrec
        return $ Q.VEBinApp f l r

    Q.VEUnApp f rec         -> do
        e <- convertEEBaseRec rec
        return $ Q.VEUnApp f e

    Q.VENot rec             -> do
        e <- convertEEBaseRec rec
        return $ Q.VENot e

    Q.VEExists q            -> return $ Q.VEExists q
    Q.VEIn rec q            -> do
        e <- convertEEBaseRec rec
        return $ Q.VEIn e q
    Q.VECase crec trec erec -> do
        c <- convertEEBaseRec crec
        t <- convertEEBaseRec trec
        e <- convertEEBaseRec erec

        return $ Q.VECase c t e

-- | Converts an 'Q.ExtendedExpr' to a 'Q.ColumnExpr', if possible.
convertEEtoCE :: Q.ExtendedExpr -> Maybe Q.ColumnExpr
convertEEtoCE ee = case ee of
    Q.EEBase eeb -> do
        ceb <- convertEEBaseTemplate convertEEtoCE eeb
        return $ Q.CEBase ceb
    _            -> Nothing

-- | Converts an 'Q.ExtendedExpr' to a 'Q.AggrExpr', if possible.
convertEEtoAE :: Q.ExtendedExpr -> Maybe Q.AggrExpr
convertEEtoAE ee = case ee of
    Q.EEBase eeb    -> do
        aeb <- convertEEBaseTemplate convertEEtoAE eeb
        return $ Q.AEBase aeb

    Q.EEAggrExpr ae -> return ae

    _               -> Nothing

-- | Shorthand to make an unprefixed column.
mkCol :: String
      -> Q.ValueExprTemplate a
mkCol c = Q.VEColumn c Nothing

appendToWhere :: Q.ColumnExpr -- ^ The expression added with logical and.
              -> Q.SelectStmt -- ^ The select statement to add to.
              -> Q.SelectStmt -- ^ The result.
appendToWhere cond select =
    select { Q.whereClause = cond : Q.whereClause select }

appendAllToWhere :: [Q.ColumnExpr]
                 -> Q.SelectStmt
                 -> Q.SelectStmt
appendAllToWhere conds select =
    select { Q.whereClause = conds ++ Q.whereClause select }

mkFromPartVar :: Int
              -> String
              -> Maybe [String]
              -> Q.FromPart
mkFromPartVar identifier = Q.FPAlias (Q.FEVariable identifier)

-- | Translate 'A.JoinRel' into 'Q.BinaryFunction'.
translateJoinRel :: A.JoinRel
                 -> Q.BinaryFunction
translateJoinRel rel = case rel of
    A.EqJ -> Q.BFEqual
    A.GtJ -> Q.BFGreaterThan
    A.GeJ -> Q.BFGreaterEqual
    A.LtJ -> Q.BFLowerThan
    A.LeJ -> Q.BFLowerEqual
    A.NeJ -> Q.BFNotEqual

translateAggrType :: A.AggrType
                  -> (Q.AggregateFunction, Maybe A.Expr)
translateAggrType aggr = case aggr of
    A.Avg e  -> (Q.AFAvg, Just e)
    A.Max e  -> (Q.AFMax, Just e)
    A.Min e  -> (Q.AFMin, Just e)
    A.Sum e  -> (Q.AFSum, Just e)
    A.Count  -> (Q.AFCount, Nothing)
    A.All e  -> (Q.AFAll, Just e)
    A.Any e  -> (Q.AFAny, Just e)

translateExprValueExprTemplate :: (Maybe [Q.SelectColumn] -> A.Expr -> a)
                               -> (Q.ValueExprTemplate a -> a)
                               -> ([Q.SelectColumn] -> String -> a)
                               -> Maybe [Q.SelectColumn]
                               -> A.Expr
                               -> a
translateExprValueExprTemplate rec wrap inline optSelectClause expr =
    case expr of
        A.IfE c t e       ->
            wrap $ Q.VECase (rec optSelectClause c)
                            (rec optSelectClause t)
                            (rec optSelectClause e)
                               
        A.BinAppE f e1 e2 ->
            wrap $ Q.VEBinApp (translateBinFun f)
                              (rec optSelectClause e1)
                              $ rec optSelectClause e2
        A.UnAppE f e      ->
            wrap $ case f of
                A.Not    -> Q.VENot tE
                A.Cast t -> Q.VECast tE $ translateATy t
                A.Sin    -> Q.VEUnApp Q.UFSin tE
                A.Cos    -> Q.VEUnApp Q.UFCos tE
                A.Tan    -> Q.VEUnApp Q.UFTan tE
                A.ASin   -> Q.VEUnApp Q.UFASin tE
                A.ACos   -> Q.VEUnApp Q.UFACos tE
                A.ATan   -> Q.VEUnApp Q.UFATan tE
                A.Sqrt   -> Q.VEUnApp Q.UFSqrt tE
                A.Log    -> Q.VEUnApp Q.UFLog tE
                A.Exp    -> Q.VEUnApp Q.UFExp tE

          where
            tE = rec optSelectClause e

        A.ColE n          -> case optSelectClause of
            Just s  -> inline s n
            Nothing -> wrap $ mkCol n
        A.ConstE v        -> wrap $ Q.VEValue $ translateAVal v

translateExprCE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.ColumnExpr
translateExprCE = translateExprValueExprTemplate translateExprCE Q.CEBase inlineCE

translateExprEE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.ExtendedExpr
translateExprEE = translateExprValueExprTemplate translateExprEE Q.EEBase inlineEE

translateBinFun :: A.BinFun -> Q.BinaryFunction
translateBinFun f = case f of
    A.Gt        -> Q.BFGreaterThan
    A.Lt        -> Q.BFLowerThan
    A.GtE       -> Q.BFGreaterEqual
    A.LtE       -> Q.BFLowerEqual
    A.Eq        -> Q.BFEqual
    A.NEq       -> Q.BFNotEqual
    A.And       -> Q.BFAnd
    A.Or        -> Q.BFOr
    A.Plus      -> Q.BFPlus
    A.Minus     -> Q.BFMinus
    A.Times     -> Q.BFTimes
    A.Div       -> Q.BFDiv
    A.Modulo    -> Q.BFModulo
    A.Contains  -> Q.BFContains
    A.SimilarTo -> Q.BFSimilarTo
    A.Like      -> Q.BFLike
    A.Concat    -> Q.BFConcat

-- | Translate sort information into '[Q.WindowOrderExpr]', using the column
-- function, which takes a 'String'.
translateSortInf :: [A.SortAttr]
                 -> (String -> Q.AggrExpr)
                 -> [Q.WindowOrderExpr]
translateSortInf sortInfos colFun = map toWOE sortInfos
  where
    toWOE (n, d) = Q.WOE (colFun n) $ translateSortDir d


-- | Translate a single join condition into it's 'Q.ColumnExpr' equivalent.
-- 'A.Expr' contained within the join condition are inlined with the according
-- select clauses.
translateJoinCond :: [Q.SelectColumn] -- ^ Left select clause.
                  -> [Q.SelectColumn] -- ^ Right select clause.
                  -> (A.Expr, A.Expr, A.JoinRel)
                  -> Q.ColumnExpr
translateJoinCond lSelectClause rSelectClause (l, r, j) =
    Q.CEBase $ Q.VEBinApp (translateJoinRel j)
                          (translateExprCE (Just lSelectClause) l)
                          (translateExprCE (Just rSelectClause) r)

translateSortDir :: A.SortDir -> Q.SortDirection
translateSortDir d = case d of
    A.Asc  -> Q.Ascending
    A.Desc -> Q.Descending

translateAVal :: A.AVal -> Q.Value
translateAVal v = case v of
    A.VInt i    -> Q.VInteger i
    A.VStr s    -> Q.VText s
    A.VBool b   -> Q.VBoolean b
    A.VDouble d -> Q.VDoublePrecision d
    A.VDec d    -> Q.VDecimal d
    A.VNat n    -> Q.VInteger n

translateATy :: A.ATy -> Q.DataType
translateATy t = case t of
    A.AInt    -> Q.DTInteger
    A.AStr    -> Q.DTText
    A.ABool   -> Q.DTBoolean
    A.ADec    -> Q.DTDecimal
    A.ADouble -> Q.DTDoublePrecision
    A.ANat    -> Q.DTInteger

