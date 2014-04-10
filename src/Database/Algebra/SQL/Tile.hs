{-# LANGUAGE DoAndIfThenElse #-}
module Database.Algebra.SQL.Tile
    ( TileTree (TileNode, ReferenceLeaf)
    , TileChildren
    , ExternalReference
    , InternalReference
    , DependencyList
    , TransformResult
    , transform
    , PFDag
    ) where

-- TODO maybe split this file into the tile definition
--      and the transform things.
-- TODO embed closing tiles as subqueries (are there any sub queries which are
-- correlated?)? (reader?)
-- TODO isMultiReferenced special case: check for same parent !!

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Trans.RWS.Strict
import qualified Data.IntMap as IntMap
import Data.Maybe
import qualified Data.DList as DL
    ( DList
    , singleton
    )

import qualified Database.Algebra.Dag as D
import qualified Database.Algebra.Dag.Common as C
import qualified Database.Algebra.Pathfinder.Data.Algebra as A

import qualified Database.Algebra.SQL.Query as Q
import Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkSubQuery
    , mkPCol
    , affectsSortOrderAE
    , affectsSortOrderCE
    )

-- | A tile internal reference type.
type InternalReference = Q.ReferenceType

-- | The type used to reference table expressions outside of a tile.
type ExternalReference = Int

-- | Aliased tile children, where the first part is the alias used within the
-- 'Q.SelectStmt'.
type TileChildren = [(InternalReference, TileTree)]

-- | Defines the tile tree structure.
data TileTree = -- | A tile: The first argument determines whether the
                -- 'Q.SelectStmt' has nothing else but the select, from or
                -- where clause set and is thus mergeable.
                TileNode Bool Q.SelectStmt TileChildren
                -- | A reference pointing to another TileTree: The second
                -- argument specifies the columns of the referenced table
                -- expression.
              | ReferenceLeaf ExternalReference [String]

-- | The type of DAG used by Pathfinder.
type PFDag = D.AlgebraDag A.PFAlgebra

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
type TransformMonad = RWS PFDag DependencyList TransformState

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
                  -> PFDag                      -- ^ The used DAG.
                  -> TransformState             -- ^ The inital state.
                  -> (a, DependencyList)
runTransformMonad = evalRWS 

-- | Check if node has more than one parent.
isMultiReferenced :: C.AlgNode
                  -> PFDag
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

-- | Transform a 'PFDag', while swapping out repeatedly used sub expressions
-- (nodes with more than one parent).
-- A 'PFDag' can have multiple root nodes, and therefore the function returns a
-- list of root tiles and their dependencies.
transform :: PFDag -> TransformResult
transform dag = runTransformMonad result dag sInitial
  where rootNodes = D.rootNodes dag
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
            (C.TerOp () _ _ _)  ->
                ( True
                , fail "transformOperator: invalid operator type TerOp found"
                )


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

    let sFun n   = Q.SCAlias (Q.AEBase $ mkPCol alias n) n
        sClause  = map (sFun . fst) schema
        castedNull ty = Q.CEBase $ Q.VECast (Q.CEBase $ Q.VEValue Q.VNull)
                                            (translateATy ty)
        fLiteral = Q.FPAlias (Q.FESubQuery $ Q.VQLiteral [map (castedNull . snd) schema])
                             alias
                             $ Just $ map fst schema

    return $ TileNode
             True
             emptySelectStmt
             { Q.selectClause = sClause
             , Q.fromClause = [fLiteral]
             , Q.whereClause = [Q.CEBase . Q.VEValue $ Q.VBoolean False]
             }
             []
transformNullaryOp (A.LitTable tuples schema) = do
    alias <- generateAliasName

    let sFun n   = Q.SCAlias (Q.AEBase $ mkPCol alias n) n
        sClause  = map (sFun . fst) schema
        fLiteral = Q.FPAlias (Q.FESubQuery $ Q.VQLiteral $ map tMap tuples)
                             alias
                             $ Just $ map fst schema

    return $ TileNode
             True
             emptySelectStmt
             { Q.selectClause = sClause
             , Q.fromClause = [fLiteral]
             }
             []
  where tMap = map $ Q.CEBase . Q.VEValue . translateAVal 

transformNullaryOp (A.TableRef (name, info, _))   = do
    alias <- generateAliasName
    
    let f (n, _) = Q.SCAlias (Q.AEBase $ mkPCol alias n) n
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

    return $ TileNode True body []


-- | Abstraction for rank operators.
transformUnOpRank :: -- AdvancedExpr constructor.
                     ([Q.OrderExpr] -> Q.AdvancedExpr)
                  -> (String, [A.SortAttr])
                  -> C.AlgNode
                  -> TransformMonad TileTree
transformUnOpRank rankConstructor (name, sortList) =
    let colFun sClause = Q.SCAlias
                         ( rankConstructor $ asOrderExprList
                                             sClause
                                             sortList
                         )
                         name
    in attachColFunUnOp colFun (TileNode False)

transformUnOp :: A.UnOp -> C.AlgNode -> TransformMonad TileTree
transformUnOp (A.Serialize (mDescr, pos, payloadCols)) c = do

    (select, children) <- transformAsSelectStmt c

    let sClause              = Q.selectClause select
        inline               = inlineAE sClause
        project (col, alias) = Q.SCAlias (inline col) alias
        itemi i              = "item" ++ show i
        payloadProjs         =
            zipWith (\(A.PayloadCol col) i -> Q.SCAlias (inline col) (itemi i))
                    payloadCols
                    [1..]

    return $ TileNode
             False
             select
             { Q.selectClause = map project (descrProjList ++ posProjList)
                                ++ payloadProjs
             , -- Order by optional columns. Remove constant column expressions,
               -- since SQL99 defines different semantics.
               Q.orderByClause =
                   map (flip Q.OE Q.Ascending)
                       $ descrColAdder . discardConstAdvancedExprs
                         $ map inline posOrderList
             }
             children
  where
    (descrColAdder, descrProjList) = case mDescr of
        Nothing               -> (id, [])
        -- Project and sort. Since descr gets added as new alias we can use it
        -- in the ORDER BY clause.
        Just (A.DescrCol col) -> ( (:) $ Q.AEBase $ Q.VEColumn "descr" Nothing
                                 , [(col, "descr")]
                                 )

    (posOrderList, posProjList)     = case pos of
        A.NoPos       -> ([], [])
        -- Sort and project.
        A.AbsPos col  -> (["pos"], [(col, "pos")])
        -- Sort but do not project.
        A.RelPos cols -> (cols, [])


transformUnOp (A.RowNum (name, sortList, optPart)) c =
    attachColFunUnOp colFun (TileNode False) c
  where colFun sClause = Q.SCAlias rowNumExpr name
          where rowNumExpr = Q.AERowNum
                             (liftM (inlineAE sClause) optPart)
                             $ asOrderExprList sClause sortList

transformUnOp (A.RowRank inf) c = transformUnOpRank Q.AEDenseRank inf c
transformUnOp (A.Rank inf) c = transformUnOpRank Q.AERank inf c
transformUnOp (A.Project projList) c = do
    
    (select, children) <- transformAsOpenSelectStmt c

    let sClause  = Q.selectClause select
        -- Inlining is obligatory here, since we possibly eliminate referenced
        -- columns. ('translateExpr' inlines columns.)
        f (n, e) = Q.SCAlias tE n
          where tE = translateExprAE (Just sClause) e

    return $ TileNode
             True
             -- Replace the select clause with the projection list.
             select { Q.selectClause = map f projList }
             -- But use the old children.
             children


transformUnOp (A.Select expr) c = do

    (select, children) <- transformAsOpenSelectStmt c
    
    return $ TileNode
             True
             ( appendToWhere ( translateExprCE
                               (Just $ Q.selectClause select)
                               expr
                             )
               select
             )
             children

transformUnOp (A.Distinct ()) c = do

    (select, children) <- transformAsOpenSelectStmt c

    -- Keep everything but set distinct.
    return $ TileNode False select { Q.distinct = True } children

transformUnOp (A.Aggr (aggrs, partExprMapping)) c = do
    
    (select, children) <- transformAsOpenSelectStmt c

    let sClause          = Q.selectClause select
        translateE       = translateExprCE $ Just sClause
        maybeTranslateE  = liftM translateE
        -- Inlining here is obligatory, since we could eliminate referenced
        -- columns. (This is similar to projection.)
        aggrToAE (a, n)  = Q.SCAlias ( let (fun, optExpr) = translateAggrType a
                                      in Q.AEAggregate (maybeTranslateE optExpr)
                                                       fun
                                    )
                                    n

        partColumnExprs  = map (second translateE) partExprMapping
        partAdvancedExpr = map (second $ translateExprAE $ Just sClause)
                           partExprMapping


        wrapSCAlias (name, advancedExpr)
                        =
            Q.SCAlias advancedExpr name

    return $ TileNode
             False
             select
             { Q.selectClause =
                   map wrapSCAlias partAdvancedExpr ++ map aggrToAE aggrs
             , -- Since SQL treats numbers in the group by clause as column
               -- indices, filter them out. (They do not change the semantics
               -- anyway.)
               Q.groupByClause = discardConstColumnExprs $ map snd partColumnExprs
             }
             children

-- | Generates a new 'TileTree' by attaching a column, generated by a function
-- taking the select clause.
attachColFunUnOp :: ([Q.SelectColumn] -> Q.SelectColumn)
                 -> (Q.SelectStmt -> TileChildren -> TileTree)
                 -> C.AlgNode
                 -> TransformMonad TileTree
attachColFunUnOp colFun ctor child = do

    (select, children) <- transformAsOpenSelectStmt child

    let sClause = Q.selectClause select
    return $ ctor
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
    (select0, children0) <- transformAsSelectStmt c0
    (select1, children1) <- transformAsSelectStmt c1

    alias <- generateAliasName

    -- Take the schema of the first one, but could also be from the second one,
    -- since we assume they are equal.
    let schema = getSchemaSelectStmt select0

    return $ TileNode True
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

-- | Perform a cross join between two nodes.
transformBinCrossJoin :: C.AlgNode
                      -> C.AlgNode
                      -> TransformMonad TileTree
transformBinCrossJoin c0 c1 = do
    (select0, children0) <- transformAsOpenSelectStmt c0
    (select1, children1) <- transformAsOpenSelectStmt c1

    -- We can simply concatenate everything, because all things are prefixed and
    -- cross join is associative.
    return $ TileNode True
                      -- Mergeable tiles are guaranteed to have at most a
                      -- select, from and where clause. (And since
                      -- 'tileToOpenSelectStmt' does this, we always get at most that
                      -- structure.)
                      emptySelectStmt
                      { Q.selectClause =
                            Q.selectClause select0 ++ Q.selectClause select1
                      , Q.fromClause =
                            Q.fromClause select0 ++ Q.fromClause select1
                      , Q.whereClause = Q.whereClause select0
                                        ++ Q.whereClause select1
                      }
                      -- Removing duplicates is not efficient here (since it
                      -- needs substitution on non-self joins).
                      -- Will be done automatically later on.
                      $ children0 ++ children1

-- | Perform a corss join with two nodes and get a select statement from the
-- result.
transformCJToSelect :: C.AlgNode
                    -> C.AlgNode
                    -> TransformMonad (Q.SelectStmt, TileChildren)
transformCJToSelect c0 c1 = do
    cTile <- transformBinCrossJoin c0 c1
    tileToOpenSelectStmt cTile

transformBinOp :: A.BinOp
               -> C.AlgNode
               -> C.AlgNode
               -> TransformMonad TileTree
transformBinOp (A.Cross ()) c0 c1 = transformBinCrossJoin c0 c1

transformBinOp (A.EqJoin (lName, rName)) c0 c1 = do

    (select, children) <- transformCJToSelect c0 c1

    let sClause = Q.selectClause select
        cond    = mkEqual (inlineCE sClause lName)
                          $ inlineCE sClause rName

    return $ TileNode True (appendToWhere cond select) children


transformBinOp (A.ThetaJoin conditions) c0 c1  = do

    (select, children) <- transformCJToSelect c0 c1

    -- Is there at least one join conditon?
    if null conditions
    then return $ TileNode True select children
    else do

        let sClause = Q.selectClause select
            cond    = foldr mkAnd (head conds) (tail conds)
            conds   = map f conditions
            f       = translateInlinedJoinCond sClause sClause

        return $ TileNode True (appendToWhere cond select) children

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
transformExistsJoin conditions c0 c1 existsWrapF = case result of
    (Nothing, _)      -> do
        -- TODO in case we do not have merge conditions we can simply use the
        -- unmergeable but less nested select stmt on the right side
        (select0, children0) <- transformAsOpenSelectStmt c0
        (select1, children1) <- transformAsOpenSelectStmt c1

        let outerCond   = existsWrapF
                          . Q.CEBase
                          . Q.VEExists
                          $ Q.VQSelect innerSelect
            innerSelect = foldr appendToWhere select1 innerConds
            innerConds  = map f conditions
            f           = translateInlinedJoinCond (Q.selectClause select0)
                                                $ Q.selectClause select1

        return $ TileNode True
                        (appendToWhere outerCond select0)
                        $ children0 ++ children1
    (Just (l, r), cs) -> do
        (select0, children0) <- transformAsOpenSelectStmt c0
        (select1, children1) <- transformAsOpenSelectStmt c1
       
        let -- Embedd the right query into the where clause of the left one.
            leftCond    = existsWrapF $ Q.CEBase $ Q.VEIn (inlineCE lSClause l)
                                                   $ Q.VQSelect rightSelect
            -- Embedd all conditions in the right select, and set select clause
            -- to the right part of the equal join condition.
            rightSelect = (foldr f select1 cs) { Q.selectClause = [rightSCol] }
            f           = appendToWhere . translateInlinedJoinCond lSClause rSClause
            rightSCol   = Q.SCAlias (inlineAE rSClause r) r
            lSClause    = Q.selectClause select0
            rSClause    = Q.selectClause select1

        return $ TileNode True
                          (appendToWhere leftCond select0)
                          $ children0 ++ children1
  where
    result                                = foldr tryIn (Nothing, []) conditions
    -- Tries to extract a join condition for usage in the IN sql construct.
    tryIn c (Just eqCols, r)              = (Just eqCols, c:r)
    tryIn c@(left, right, j) (Nothing, r) = case j of
        A.EqJ -> (Just (left, right), r)
        _     -> (Nothing, c:r)

-- | Transform a vertex and return it as a mergeable select statement.
transformAsOpenSelectStmt :: C.AlgNode
                          -> TransformMonad (Q.SelectStmt, TileChildren) 
transformAsOpenSelectStmt n = do
    tile <- transformNode n
    tileToOpenSelectStmt tile

-- | Transform a vertex and return it as a select statement, without regard to
-- mergability.
transformAsSelectStmt :: C.AlgNode
                      -> TransformMonad (Q.SelectStmt, TileChildren)
transformAsSelectStmt n = do
    tile <- transformNode n
    tileToSelectStmt tile

-- | Converts a 'TileTree' into a select statement, inlines if possible.
-- Select statements produced by this function are mergeable, which means they
-- contain at most a select, from and where clause and have distinct set to
-- tile.
tileToOpenSelectStmt :: TileTree
                     -- The resulting 'SelectStmt' and used children (if the
                     -- 'TileTree' could not be inlined or had children itself).
                     -> TransformMonad (Q.SelectStmt, TileChildren)
tileToOpenSelectStmt t = case t of
    -- The only thing we are able to merge.
    TileNode True body children  -> return (body, children)
    -- Embed as sub query.
    TileNode False body children -> do
        alias <- generateAliasName

        let schema = getSchemaSelectStmt body

        return ( emptySelectStmt
                 { Q.selectClause =
                       columnsFromSchema alias schema
                 , Q.fromClause =
                       [mkSubQuery body alias $ Just schema]
                 }
               , children
               )
    -- Asign name and produce a 'SelectStmt' which uses it. (Let the
    -- materialization strategy handle it.)
    ReferenceLeaf r s            -> embedExternalReference r s

tileToSelectStmt :: TileTree
                 -> TransformMonad (Q.SelectStmt, TileChildren)
tileToSelectStmt t = case t of
    TileNode _ body children -> return (body, children)
    ReferenceLeaf r s        -> embedExternalReference r s

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
    Q.SCAlias (Q.AEBase $ mkPCol prefix columnName) columnName

translateInlinedJoinCond :: [Q.SelectColumn] -- ^ Left select clause.
                         -> [Q.SelectColumn] -- ^ Right select clause.
                         -> (A.LeftAttrName, A.RightAttrName, A.JoinRel)
                         -> Q.ColumnExpr
translateInlinedJoinCond lSClause rSClause j =
    translateJoinCond j (inlineCE lSClause) (inlineCE rSClause)

-- Remove all 'Q.ColumnExpr's which are constant (i.e. do not depend on a
-- column).
discardConstColumnExprs :: [Q.ColumnExpr] -> [Q.ColumnExpr]
discardConstColumnExprs = filter affectsSortOrderCE

-- Remove all 'Q.AdvancedExpr's which are constant (i.e. do not depend on a
-- column).
discardConstAdvancedExprs :: [Q.AdvancedExpr] -> [Q.AdvancedExpr]
discardConstAdvancedExprs = filter affectsSortOrderAE

-- Translates a '[A.SortAttr]' into a '[Q.OrderExpr]'. Column names will be
-- inlined as an 'Q.AdvancedExpr', constant ones will be discarded.
asOrderExprList :: [Q.SelectColumn]
                -> [A.SortAttr]
                -> [Q.OrderExpr]
asOrderExprList sClause si =
    filter (affectsSortOrderAE . Q.oExpr)
           $ translateSortInf si (inlineAE sClause)

-- | Search the select clause for a specific column definition and return it as
-- 'Q.ColumnExpr'.
inlineCE :: [Q.SelectColumn]
         -> String
         -> Q.ColumnExpr
inlineCE selectClause col =
    fromMaybe (Q.CEBase $ Q.VEColumn col Nothing)
              $ convertAEToCE $ inlineAE selectClause col

-- | Search the select clause for a specific column definition and return it as
-- 'Q.AdvancedExpr'.
inlineAE :: [Q.SelectColumn]
         -> String
         -> Q.AdvancedExpr
inlineAE selectClause col =
    fromMaybe (Q.AEBase $ mkCol col) $ foldr f Nothing selectClause
  where
    f (Q.SCAlias ae a) r = if col == a then return ae
                                       else r

-- | Converts an 'Q.AdvancedExpr' to a 'Q.ColumnExpr', if possible.
convertAEToCE :: Q.AdvancedExpr -> Maybe Q.ColumnExpr
convertAEToCE ae = case ae of
    Q.AEBase aeb -> do
        ceb <- convertBase aeb
        return $ Q.CEBase ceb
    _            -> Nothing
  where
    convertBase :: Q.AdvancedExprBase
                -> Maybe Q.ColumnExprBase
    convertBase abt = case abt of
        Q.VEValue v             -> return $ Q.VEValue v
        Q.VEColumn n p          -> return $ Q.VEColumn n p
        Q.VECast rec t          -> do
            e <- convertAEToCE rec
            return $ Q.VECast e t

        Q.VEBinApp f lrec rrec  -> do
            l <- convertAEToCE lrec
            r <- convertAEToCE rrec
            return $ Q.VEBinApp f l r

        Q.VEUnApp f rec         -> do
            e <- convertAEToCE rec
            return $ Q.VEUnApp f e

        Q.VENot rec             -> do
            e <- convertAEToCE rec
            return $ Q.VENot e

        Q.VEExists q            -> return $ Q.VEExists q
        Q.VEIn rec q            -> do
            e <- convertAEToCE rec
            return $ Q.VEIn e q
        Q.VECase crec trec erec -> do
            c <- convertAEToCE crec
            t <- convertAEToCE trec
            e <- convertAEToCE erec

            return $ Q.VECase c t e

-- | Shorthand to make an unprefixed column.
mkCol :: String
      -> Q.ValueExprTemplate a
mkCol c = Q.VEColumn c Nothing

-- | Shorthand to apply the equal function to column expressions.
mkEqual :: Q.ColumnExpr
        -> Q.ColumnExpr
        -> Q.ColumnExpr
mkEqual a b = Q.CEBase $ Q.VEBinApp Q.BFEqual a b

mkAnd :: Q.ColumnExpr
      -> Q.ColumnExpr
      -> Q.ColumnExpr
mkAnd a b = Q.CEBase $ Q.VEBinApp Q.BFAnd a b

appendToWhere :: Q.ColumnExpr -- ^ The expression added with logical and.
              -> Q.SelectStmt -- ^ The select statement to add to.
              -> Q.SelectStmt -- ^ The result.
appendToWhere cond select =
    select { Q.whereClause = cond : Q.whereClause select }

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
        A.IfE c t e        ->
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

translateExprAE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.AdvancedExpr
translateExprAE = translateExprValueExprTemplate translateExprAE Q.AEBase inlineAE

translateBinFun :: A.BinFun -> Q.BinaryFunction
translateBinFun f = case f of
    A.Gt        -> Q.BFGreaterThan
    A.Lt        -> Q.BFLowerThan
    A.GtE       -> Q.BFGreaterEqual
    A.LtE       -> Q.BFLowerEqual
    A.Eq        -> Q.BFEqual
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

-- | Translate sort information into '[Q.OrderExpr]', using the column
-- function, which takes a 'String'.
translateSortInf :: [A.SortAttr]
                 -> (String -> Q.AdvancedExpr)
                 -> [Q.OrderExpr]
translateSortInf si colFun = map f si
    where f (n, d) = Q.OE (colFun n) $ translateSortDir d


-- | Translate a single join condition into it's 'Q.ColumnExpr' equivalent.
translateJoinCond :: (A.LeftAttrName, A.RightAttrName, A.JoinRel)
                  -> (String -> Q.ColumnExpr) -- ^ Left column function.
                  -> (String -> Q.ColumnExpr) -- ^ Right column function.
                  -> Q.ColumnExpr
translateJoinCond (l, r, j) lColFun rColFun =
    Q.CEBase $ Q.VEBinApp (translateJoinRel j) (lColFun l) (rColFun r)

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

