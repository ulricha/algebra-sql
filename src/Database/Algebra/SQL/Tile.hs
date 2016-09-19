{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Algebra.SQL.Tile
    ( TileTree (TileNode, ReferenceLeaf)
    , TileChildren
    , ExternalReference
    , InternalReference
    , TileDep
    , tilePlan
    , TADag
    ) where

-- TODO maybe split this file into the tile definition
--      and the transform things.
-- TODO embed closing tiles as subqueries (are there any sub queries which are
-- correlated?)? (reader?)
-- TODO isMultiReferenced special case: check for same parent !!

import           Control.Arrow                    (second)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.IntMap                      as IntMap
import qualified Data.List                        as L
import           Data.Maybe
import           GHC.Exts                         hiding (inline)

import qualified Database.Algebra.Dag             as D
import qualified Database.Algebra.Dag.Common      as C
import           Database.Algebra.Impossible
import qualified Database.Algebra.Table.Lang      as A

import           Database.Algebra.SQL.Dialect
import qualified Database.Algebra.SQL.Query       as Q
import           Database.Algebra.SQL.Query.Util
import           Database.Algebra.SQL.Termination

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

data TileEnv = TileEnv
    { tDag     :: TADag
    , tDialect :: Dialect
    }

-- | A dependency between two tiles
type TileDep = (ExternalReference, TileTree)

-- | A combination of types which need to be modified state wise while
-- transforming:
--     * The processed nodes with multiple parents.
--
--     * The current state of the table id generator.
--
--     * The current state of the variable id generator.
--
data TileState = TS
    { multiParentNodes :: IntMap.IntMap ( ExternalReference , [String])
    , tableIdGen       :: ExternalReference
    , aliasIdGen       :: Int
    , varIdGen         :: InternalReference
    , depList          :: [TileDep]
    }

-- | The initial state.
sInitial :: TileState
sInitial = TS { multiParentNodes = IntMap.empty
              , tableIdGen       = 0
              , aliasIdGen       = 0
              , varIdGen         = 0
              , depList          = []
              }

sAddTileDep :: ExternalReference -> TileTree -> TileState -> TileState
sAddTileDep r t s = s { depList = (r, t) : (depList s) }

-- | Adds a new binding to the state.
sAddBinding :: C.AlgNode          -- ^ The key as a node with multiple parents.
            -> ( ExternalReference
               , [String]
               )                  -- ^ Name of the reference and its columns.
            -> TileState
            -> TileState
sAddBinding node t st =
    st { multiParentNodes = IntMap.insert node t $ multiParentNodes st}

-- | Tries to look up a binding for a node.
sLookupBinding :: C.AlgNode
               -> TileState
               -> Maybe (ExternalReference, [String])
sLookupBinding n = IntMap.lookup n . multiParentNodes

-- | The 'TileM' monad is used for transforming DAGs into dense tiles, it
-- is built from:
--
--     * A reader for the DAG
--
--     * A state for generating fresh names and maintain the mapping of nodes
--
type TileM a = ReaderT TileEnv (State TileState) a

-- | A table expression id generator using the state within the
-- 'TileM' type.
freshTableId :: TileM ExternalReference
freshTableId = do
    st <- get

    let tid = tableIdGen st

    put $ st { tableIdGen = succ tid }

    return tid

freshAlias :: TileM String
freshAlias = do
    st <- get

    let aid = aliasIdGen st

    put $ st { aliasIdGen = succ aid }

    return $ 'a' : show aid

-- | A variable identifier generator.
freshVariableId :: TileM InternalReference
freshVariableId = do
    st <- get

    let vid = varIdGen st

    put $ st { varIdGen = succ vid }

    return vid

-- | Check if node has more than one parent.
isMultiReferenced :: C.AlgNode -> TADag -> Bool
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

-- | Transform a 'TADag', while swapping out repeatedly used sub expressions
-- (nodes with more than one parent).
-- A 'TADag' can have multiple root nodes, and therefore the function returns a
-- list of root tiles and their dependencies.
tilePlan :: Dialect -> TADag -> ([TileTree], [TileDep])
tilePlan dialect dag = (tiles, reverse $ depList sFinal)
  where
    rootNodes       = D.rootNodes dag
    tileComp        = mapM tileNode rootNodes
    env             = TileEnv dag dialect
    (tiles, sFinal) = runState (runReaderT tileComp env) sInitial

-- | This function basically checks for already referenced nodes with more than
-- one parent, returning a reference to already computed 'TileTree's.
tileNode :: C.AlgNode -> TileM TileTree
tileNode n = do

    op <- asks $ D.operator n . tDag

    -- allowBranch indicates whether multi reference nodes shall be split
    -- for this operator, resulting in multiple equal branches. (Treeify)
    let (allowBranch, tileOp) = case op of
            -- Ignore branching for nullary operators.
            (C.NullaryOp nop)   -> (False, tileNullaryOp nop)
            (C.UnOp uop c)      -> (True, tileUnOp uop c)
            (C.BinOp bop c0 c1) ->
                case bop of
                    A.ThetaJoin _ -> (False, tileBinOp bop c0 c1)
                    _             -> (True, tileBinOp bop c0 c1)
            (C.TerOp () _ _ _)  -> $impossible

    multiRef <- asks $ isMultiReferenced n . tDag

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

                resultingTile <- tileOp

                -- Generate a name for the sub tree.
                tableId <- freshTableId

                -- Add the tree to the writer.
                modify $ sAddTileDep tableId resultingTile

                let schema = getSchemaTileTree resultingTile

                -- Add binding for this node (to prevent recalculation).
                modify $ sAddBinding n (tableId, schema)

                return $ ReferenceLeaf tableId schema
    else tileOp

tileNullaryOp :: A.NullOp -> TileM TileTree
tileNullaryOp (A.LitTable (tuples, typedSchema)) = do
    tableAlias <- freshAlias

    let -- Abstracts over the differences.
        tile otherFeatures tuples' wClause =
            TileNode (otherFeatures <> tableF)
                     emptySelectStmt
                     { Q.selectClause = columnsFromSchema tableAlias schema
                     , Q.fromClause   =
                         [ Q.FPAlias (Q.FESubQuery $ Q.VQLiteral tuples')
                                     tableAlias
                                     $ Just schema
                         ]
                     , Q.whereClause  = wClause
                     }
                     []

    return $ case tuples of
        [] -> tile (colProjectF <> filterF)
                   [map (castedNull . snd) typedSchema]
                   [Q.CEBase . Q.VEValue $ Q.VBoolean False]
        _  -> tile colProjectF
                   (map (map translateLit) tuples)
                   []
  where
    schema        = map fst typedSchema
    castedNull ty = Q.CEBase $ Q.VEUnApp (Q.UFCast $ translateATy ty)
                                         (Q.CEBase $ Q.VEValue Q.VNull)
    translateLit  = Q.CEBase . Q.VEValue . translateAVal

tileNullaryOp (A.TableRef (name, typedSchema, _))   = do
    tableAlias <- freshAlias

    return $ TileNode (colProjectF <> tableF)
                      emptySelectStmt
                      { -- Map the columns of the table reference to the given
                        -- column names.
                        Q.selectClause = columnsFromSchema tableAlias schema
                      , Q.fromClause   =
                              [ Q.FPAlias (Q.FETableReference name)
                                          tableAlias
                                          -- Map to old column name.
                                          $ Just schema
                              ]
                      }
                      []
  where
    schema = map fst typedSchema

-- | Abstraction for rank operators.
tileUnOpRank :: -- ExtendedExpr constructor.
                     Q.WindowFunction
                  -> (String, [A.SortSpec])
                  -> C.AlgNode
                  -> TileM TileTree
tileUnOpRank rankFun (name, sortList) =
    attachColFunUnOp colFun $ exprProjectF <> windowFunctionF
  where
    colFun sClause = Q.SCAlias rankExpr name

      where
        rankExpr = Q.EEWinFun rankFun
                              []
                              (asWindowOrderExprList sClause sortList)
                              Nothing

tileUnOp :: A.UnOp -> C.AlgNode -> TileM TileTree
tileUnOp (A.Serialize (ref, key, ord, items)) c = do
    (ctor, select, children) <- terminateTile c $ exprProjectF <> sortF

    let -- Inlining is obligatory here, since we possibly eliminate referenced
        -- columns. ('translateExpr' inlines columns.)
        translateAlias :: (A.Attr, A.Expr) -> Q.SelectColumn
        translateAlias (col, expr) = Q.SCAlias translatedExpr col
          where
            translatedExpr = translateExprEE (Just $ Q.selectClause select) expr

    let sortExprs = [ Q.OE (Q.EEBase $ mkCol rc) Q.Ascending
                    | A.RefCol rc _ <- ref
                    ]
                    ++
                    [ Q.OE (Q.EEBase $ mkCol oc)
                           (translateSortDir d)
                    | A.OrdCol (oc, d) _ <- ord
                    ]

        projs      =    [ translateAlias (rc, e) | A.RefCol rc e <- ref ]
                     ++ [ translateAlias (kc, e) | A.KeyCol kc e <- key ]
                     ++ [ translateAlias (oc, e) | A.OrdCol (oc, _) e <- ord ]
                     ++ [ translateAlias (pc, e) | A.PayloadCol pc e <- items]

    return $ ctor
             select { Q.selectClause = projs , Q.orderByClause = sortExprs }
             children

tileUnOp (A.RowNum (name, sortList, partExprs)) c =
    attachColFunUnOp colFun
                     (exprProjectF <> windowFunctionF)
                     c
  where
    colFun sClause = Q.SCAlias rowNumExpr name
        where
          -- ROW_NUMBER() OVER (PARTITION BY p ORDER BY s)
          rowNumExpr = Q.EEWinFun Q.WFRowNumber
                                  nonLitPartExprs
                                  (asWindowOrderExprList sClause sortList)
                                  Nothing

          nonLitPartExprs = filter (not . Q.literalAggrExpr)
                            $ map (translateExprAE $ Just sClause) partExprs

tileUnOp (A.WinFun ((name, fun), partExprs, sortExprs, mFrameSpec)) c =
    attachColFunUnOp colFun
                     (exprProjectF <> windowFunctionF)
                     c

  where
    colFun sClause = Q.SCAlias winFunExpr name
      where
        winFunExpr = Q.EEWinFun (translateWindowFunction translateE fun)
                                (map (translateExprAE $ Just sClause) partExprs)
                                (asWindowOrderExprList sClause sortExprs)
                                (fmap translateFrameSpec mFrameSpec)

        translateE = translateExprCE $ Just sClause

tileUnOp (A.RowRank inf) c = tileUnOpRank Q.WFDenseRank inf c
tileUnOp (A.Rank inf) c = tileUnOpRank Q.WFRank inf c
tileUnOp (A.Project projList) c = do

    (ctor, select, children) <- terminateTile c exprProjectF

    let -- Inlining is obligatory here, since we possibly eliminate referenced
        -- columns. ('translateExpr' inlines columns.)
        translateAlias :: (A.Attr, A.Expr) -> Q.SelectColumn
        translateAlias (col, expr) = Q.SCAlias translatedExpr col
          where
            translatedExpr = translateExprEE (Just $ Q.selectClause select) expr

    return $ ctor select
                  -- Replace the select clause with the projection list.
                  { Q.selectClause = map translateAlias projList }
                  -- But use the old children.
                  children

tileUnOp (A.Select expr) c = do

    (ctor, select, children) <- terminateTile c filterF
    let conjuncts = map (translateExprCE (Just $ Q.selectClause select)) $ splitConjuncts expr
    let select' = foldr appendToWhere select conjuncts

    return $ ctor select' children

tileUnOp (A.Distinct ()) c = do

    (ctor, select, children) <- terminateTile c dupElimF

    -- Keep everything but set distinct.
    return $ ctor select { Q.distinct = True } children

tileUnOp (A.Aggr (aggrs, partExprMapping)) c = do

    (ctor, select, children) <- terminateTile c $ exprProjectF <> aggrAndGroupingF

    let justSClause       = Just $ Q.selectClause select
        translateE        = translateExprCE justSClause
        -- Inlining here is obligatory, since we could eliminate referenced
        -- columns. (This is similar to projection.)
        aggrToEE (a, n)   =
            Q.SCAlias ( let afun = translateAggrType translateE a
                        in Q.EEAggrExpr $ Q.AEAggregate afun
                      )
                      n

        partColumnExprs   = map (second translateE) partExprMapping
        partExtendedExprs = map (second $ translateExprEE $ justSClause)
                                partExprMapping


        wrapSCAlias (name, extendedExpr)
                          =
            Q.SCAlias extendedExpr name

    return $ ctor select
                  { Q.selectClause =
                        map wrapSCAlias partExtendedExprs
                        ++ map aggrToEE aggrs
                  , -- Since SQL treats numbers in the group by clause as
                    -- column indices, filter them out. (They do not change
                    -- the semantics anyway.)
                    Q.groupByClause =
                        filter affectsSortOrderCE $ map snd partColumnExprs
                  }
                  children

-- | Generates a new 'TileTree' by attaching a column, generated by a function
-- taking the select clause.
attachColFunUnOp :: ([Q.SelectColumn] -> Q.SelectColumn)
                 -> FeatureSet
                 -> C.AlgNode
                 -> TileM TileTree
attachColFunUnOp colFun opFeatures c = do

    (ctor, select, children) <- terminateTile c opFeatures

    let sClause = Q.selectClause select

    -- Attach a column to the select clause generated by the
    -- given function.
    return $ case colFun sClause of
                 col@(Q.SCAlias _ name) ->
                     ctor select { Q.selectClause = col : pruneCol name sClause }
                          children
                 col@Q.SCExpr{}         ->
                     ctor select { Q.selectClause = col : sClause }
                          children

pruneCol :: String -> [Q.SelectColumn] -> [Q.SelectColumn]
pruneCol n cols = filter namePred cols
  where
    namePred (Q.SCAlias _ n') | n == n' = False
    namePred _                          = True

-- | Abstracts over binary set operation operators.
tileBinSetOp :: Q.SetOperation
                  -> C.AlgNode
                  -> C.AlgNode
                  -> TileM TileTree
tileBinSetOp setOp c0 c1 = do

    -- Use one tile to get the schema information.
    (select0, children0) <- terminateSelect c0 noneF
    (select1, children1) <- terminateSelect c1 noneF

    -- Impose a canonical order on entries in the SELECT clauses to
    -- ensure that schemata of the set operator inputs match.

    -- FIXME 'Q.sName' is a partial function!
    let select0' = select0 { Q.selectClause = sortWith Q.sName $ Q.selectClause select0 }
        select1' = select1 { Q.selectClause = sortWith Q.sName $ Q.selectClause select1 }

    tableAlias <- freshAlias

    -- Take the schema of the first one, but could also be from the second one,
    -- since we assume they are equal.
    let schema = getSchemaSelectStmt select0'

    return $ TileNode (exprProjectF <> tableF)
                      emptySelectStmt
                      { Q.selectClause =
                            columnsFromSchema tableAlias schema
                      , Q.fromClause =
                            [ Q.FPAlias ( Q.FESubQuery
                                          $ Q.VQBinarySetOperation
                                            (Q.VQSelect select0')
                                            (Q.VQSelect select1')
                                            setOp
                                        )
                                        tableAlias
                                        $ Just schema
                            ]
                      }
                      $ children0 ++ children1

-- | Perform a cross join between two nodes.
tileBinCrossJoin :: C.AlgNode
                      -> C.AlgNode
                      -> TileM ( FeatureSet
                                   , Q.SelectStmt
                                   , TileChildren
                                   )
tileBinCrossJoin c0 c1 = do

    (childFeatures0, select0, children0) <- tileF c0
    (childFeatures1, select1, children1) <- tileF c1

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
    tileF c = terminateOnCollision c opFeatures
    opFeatures   = exprProjectF <> tableF <> filterF

tileBinOp :: A.BinOp
               -> C.AlgNode
               -> C.AlgNode
               -> TileM TileTree
tileBinOp (A.Cross ()) c0 c1 = do
    (f, s, c) <- tileBinCrossJoin c0 c1
    return $ TileNode f s c

tileBinOp (A.EqJoin (lName, rName)) c0 c1 = do

    (childrenFeatures, select, children) <- tileBinCrossJoin c0 c1

    let sClause = Q.selectClause select
        cond    = Q.CEBase $ Q.VEBinApp Q.BFEqual (inlineCE sClause lName)
                                                  $ inlineCE sClause rName

    -- 'tileBinCrossJoin' already has the 'filterF' feature.
    return $ TileNode childrenFeatures (appendToWhere cond select) children


tileBinOp (A.ThetaJoin conditions) c0 c1  = do

    when (null conditions) $impossible

    (childrenFeatures, select, children) <- tileBinCrossJoin c0 c1

    let sClause = Q.selectClause select
        conds   = map (translateJoinCond sClause sClause) conditions

    return $ TileNode childrenFeatures
                        (appendAllToWhere conds select)
                        children

tileBinOp (A.LeftOuterJoin conditions) c0 c1 = do
    (select0, children0) <- terminateSelect c0 noneF
    (select1, children1) <- terminateSelect c1 noneF

    let q0 = Q.FESubQuery $ Q.VQSelect select0
    let q1 = Q.FESubQuery $ Q.VQSelect select1

    fpAlias0 <- freshAlias
    fpAlias1 <- freshAlias

    let schema0 = map Q.sName $ Q.selectClause select0
    let schema1 = map Q.sName $ Q.selectClause select1

    let fp0 = Q.FPAlias q0 fpAlias0 Nothing
    let fp1 = Q.FPAlias q1 fpAlias1 Nothing

    joinAlias <- freshAlias

    let joinCond = translateExplJoinConds fpAlias0 fpAlias1 conditions
    let joinOp   = Q.LeftOuterJoin joinCond

    return $ TileNode (exprProjectF <> tableF)
                      emptySelectStmt
                          { Q.selectClause =
                              (columnsFromSchema joinAlias $ schema0 ++ schema1)
                              ++
                              (columnsFromSchema fpAlias1 schema1)
                          , Q.fromClause =
                              [ Q.FPAlias (Q.FEExplicitJoin joinOp fp0 fp1)
                                          joinAlias
                                          (Just $ schema0 ++ schema1) ]
                          }
                      (children0 ++ children1)

tileBinOp (A.SemiJoin cs) c0 c1          =
    tileExistsJoin cs c0 c1 id
tileBinOp (A.AntiJoin cs) c0 c1          =
    tileExistsJoin cs c0 c1 (Q.CEBase . Q.VEUnApp Q.UFNot)
tileBinOp (A.DisjUnion ()) c0 c1         =
    tileBinSetOp Q.SOUnionAll c0 c1
tileBinOp (A.Difference ()) c0 c1        =
    tileBinSetOp Q.SOExceptAll c0 c1

tileExistsJoin :: [(A.Expr, A.Expr, A.JoinRel)]
                    -> C.AlgNode
                    -> C.AlgNode
                    -> (Q.ColumnExpr -> Q.ColumnExpr)
                    -> TileM TileTree
tileExistsJoin conditions c0 c1 wrapFun = do

    when (null conditions) $impossible

    (ctor0, select0, children0) <- terminateTile c0 filterF

    -- Ignore operator features, since it will be nested and therefore
    -- terminated.
    (select1, children1) <- terminateSelect c1 noneF

    let ctor s = ctor0 s $ children0 ++ children1

    -- Split the conditions into the first equality condition found and the
    -- remaining ones.
    case foldr findEq (Nothing, []) conditions of
        -- We did not find an equality condition, use the EXISTS construct.
        (Nothing, _)               -> do
            -- TODO in case we do not have merge conditions we can simply use
            -- the unmergeable but less nested select stmt on the right side

            let outerCond   = wrapFun . Q.CEBase
                                      . Q.VEExists
                                      $ Q.VQSelect innerSelect
                innerSelect = appendAllToWhere innerConds select1
                innerConds  = map f conditions
                f           = translateJoinCond (Q.selectClause select0)
                                                $ Q.selectClause select1

            return $ ctor (appendToWhere outerCond select0)
        -- We did find an equality condition, use it with the IN construct.
        (Just (l, r), conditions') -> do

            let -- Embedd the right query into the where clause of the left one.
                leftCond    =
                    wrapFun . Q.CEBase
                            . Q.VEIn (translateExprCE (Just lSClause) l)
                            $ rightSelect'

                -- If the nested query is a simple selection from a
                -- literal table, use the literal table directly:
                -- SELECT t.c FROM (VALUES ...) AS t(c)
                -- =>
                -- VALUES ...
                rightSelect' =
                    case rightSelect of
                        Q.VQSelect
                          (Q.SelectStmt
                            [Q.SCExpr (Q.EEBase (Q.VEColumn colName (Just tabName)))]
                            False
                            [Q.FPAlias (Q.FESubQuery (Q.VQLiteral rows)) tabName' (Just [colName'])]
                            []
                            []
                            []) | colName == colName' && tabName == tabName' -> Q.VQLiteral rows
                        _ -> rightSelect

                -- Embedd all conditions in the right select, and set select
                -- clause to the right part of the equal join condition.
                rightSelect = Q.VQSelect $ appendAllToWhere innerConds select1
                                           { Q.selectClause = [rightSCol] }
                innerConds  = map f conditions'
                f           = translateJoinCond lSClause rSClause
                rightSCol   = Q.SCExpr (translateExprEE (Just rSClause) r)
                lSClause    = Q.selectClause select0
                rSClause    = Q.selectClause select1

            return $ ctor (appendToWhere leftCond select0)
  where
    -- Tries to extract a join condition for usage in the IN sql construct.
    findEq c (Just eqCols, r)              = (Just eqCols, c:r)
    findEq c@(left, right, j) (Nothing, r) = case j of
        A.EqJ -> (Just (left, right), r)
        _     -> (Nothing, c:r)

-- | Terminates a SQL fragment when suggested. Returns the resulting
-- 'FeatureSet' of the child, the 'Q.SelectStmt' and its children.
terminateOnCollision :: C.AlgNode
                     -> FeatureSet
                     -> TileM (FeatureSet, Q.SelectStmt, TileChildren)
terminateOnCollision n topFs = do
    terminatesOverDialect <- pure terminatesOver <*> asks tDialect
    tile                  <- tileNode n

    case tile of
        TileNode bottomFs body children
            | topFs `terminatesOverDialect` bottomFs -> do
                tableAlias <- freshAlias

                let schema = getSchemaSelectStmt body

                return ( colProjectF <> tableF
                       , emptySelectStmt
                             { Q.selectClause =
                                   columnsFromSchema tableAlias schema
                             , Q.fromClause =
                                   [mkSubQuery body tableAlias $ Just schema]
                             }
                       , children
                       )
            | otherwise                       ->
                return (bottomFs, body, children)
        ReferenceLeaf r s                     -> do
                (sel, cs) <- embedExternalReference r s
                return (colProjectF <> tableF, sel, cs)

-- | Terminate a tile if features collide. Returns the old select statement if
-- no collision occured or a fresh select statement over the terminated tile
-- otherwise.
terminateSelect :: C.AlgNode -> FeatureSet -> TileM (Q.SelectStmt, TileChildren)
terminateSelect n topFs = do
    (_, select, children) <- terminateOnCollision n topFs
    pure (select, children)

-- | Terminate a tile if feature sets collide. Returns a constructor for a new
-- tile with the combined featureset, the select statement and the tile
-- children.
terminateTile :: C.AlgNode
              -> FeatureSet
              -> TileM ( Q.SelectStmt -> TileChildren -> TileTree
                       , Q.SelectStmt
                       , TileChildren
                       )
terminateTile n topFs = do
    (fs, select, cs) <- terminateOnCollision n topFs
    return (TileNode $ fs <> topFs, select, cs)

-- | Embeds an external reference into a 'Q.SelectStmt'.
embedExternalReference :: ExternalReference
                       -> [String]
                       -> TileM (Q.SelectStmt, TileChildren)
embedExternalReference extRef schema = do

        tableAlias <- freshAlias
        varId <- freshVariableId

        return ( emptySelectStmt
                 { -- Use the schema to construct the select clause.
                   Q.selectClause =
                       columnsFromSchema tableAlias schema
                 , Q.fromClause =
                       [Q.FPAlias (Q.FEVariable varId) tableAlias $ Just schema]
                 }
               , [(varId, ReferenceLeaf extRef schema)]
               )

-- | Generate a select clause with column names from a schema and a prefix.
columnsFromSchema :: String -> [String] -> [Q.SelectColumn]
columnsFromSchema p = map $ asSelectColumn p

-- | Creates 'Q.SelectColumn' which points at a prefixed column with the same
-- name.
asSelectColumn :: String
               -> String
               -> Q.SelectColumn
asSelectColumn tablePrefix columnName =
    Q.SCAlias (Q.EEBase $ mkPCol tablePrefix columnName) columnName

-- Translates a '[A.SortSpec]' into a '[Q.WindowOrderExpr]'. Column names will
-- be inlined as a 'Q.AggrExpr', constant ones will be discarded.
asWindowOrderExprList :: [Q.SelectColumn]
                      -> [A.SortSpec]
                      -> [Q.WindowOrderExpr]
asWindowOrderExprList sClause si =
    filter (affectsSortOrderAE . Q.woExpr)
           $ translateSortInf si (translateExprAE $ Just sClause)

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
    f sc r = case sc of
        Q.SCAlias expr alias | col == alias -> return expr
        _                                   -> r

-- | Generic base converter for the value expression template. Since types do
-- not have equal functionality, conversion can fail.
convertEEBaseTemplate :: (Q.ExtendedExpr -> Maybe a)
                      -> Q.ExtendedExprBase
                      -> Maybe (Q.ValueExprTemplate a)
convertEEBaseTemplate convertEEBaseRec eeb = case eeb of
    Q.VEValue v             -> return $ Q.VEValue v
    Q.VEColumn n p          -> return $ Q.VEColumn n p
    Q.VEBinApp f lrec rrec  -> do
        l <- convertEEBaseRec lrec
        r <- convertEEBaseRec rrec
        return $ Q.VEBinApp f l r
    Q.VEUnApp f rec         -> do
        e <- convertEEBaseRec rec
        return $ Q.VEUnApp f e
    Q.VEExists q            -> return $ Q.VEExists q
    Q.VEIn rec q            -> do
        e <- convertEEBaseRec rec
        return $ Q.VEIn e q
    Q.VECase crec trec erec -> do
        c <- convertEEBaseRec crec
        t <- convertEEBaseRec trec
        e <- convertEEBaseRec erec

        return $ Q.VECase c t e
    Q.VEBetween erec lrec urec -> do
        c <- convertEEBaseRec erec
        t <- convertEEBaseRec lrec
        e <- convertEEBaseRec urec

        return $ Q.VEBetween c t e

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

splitConjuncts :: A.Expr -> [A.Expr]
splitConjuncts (A.BinAppE A.And e1 e2) = splitConjuncts e1 ++ splitConjuncts e2
splitConjuncts e                       = [e]

appendToWhere :: Q.ColumnExpr -- ^ The expression added with logical and.
              -> Q.SelectStmt -- ^ The select statement to add to.
              -> Q.SelectStmt -- ^ The result.
appendToWhere cond select =
    select { Q.whereClause = cond : Q.whereClause select }

-- | Append predicate expressions to the WHERE clause of a select
-- statement.
appendAllToWhere :: [Q.ColumnExpr]
                 -> Q.SelectStmt
                 -> Q.SelectStmt
appendAllToWhere conds select =
    select { Q.whereClause = conds ++ Q.whereClause select }

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

translateFrameSpec :: A.FrameBounds -> Q.FrameSpec
translateFrameSpec (A.HalfOpenFrame fs)  = Q.FHalfOpen $ translateFrameStart fs
translateFrameSpec (A.ClosedFrame fs fe) = Q.FClosed (translateFrameStart fs)
                                                     (translateFrameEnd fe)

translateFrameStart :: A.FrameStart -> Q.FrameStart
translateFrameStart A.FSUnboundPrec = Q.FSUnboundPrec
translateFrameStart (A.FSValPrec i) = Q.FSValPrec i
translateFrameStart A.FSCurrRow     = Q.FSCurrRow

translateFrameEnd :: A.FrameEnd -> Q.FrameEnd
translateFrameEnd A.FEUnboundFol = Q.FEUnboundFol
translateFrameEnd (A.FEValFol i) = Q.FEValFol i
translateFrameEnd A.FECurrRow    = Q.FECurrRow

translateWindowFunction :: (A.Expr -> Q.ColumnExpr) -> A.WinFun -> Q.WindowFunction
translateWindowFunction translateExpr wfun = case wfun of
    A.WinMax e        -> Q.WFMax $ translateExpr e
    A.WinMin e        -> Q.WFMin $ translateExpr e
    A.WinSum e        -> Q.WFSum $ translateExpr e
    A.WinAvg e        -> Q.WFAvg $ translateExpr e
    A.WinAll e        -> Q.WFAll $ translateExpr e
    A.WinAny e        -> Q.WFAny $ translateExpr e
    A.WinFirstValue e -> Q.WFFirstValue $ translateExpr e
    A.WinLastValue e  -> Q.WFLastValue $ translateExpr e
    A.WinCount        -> Q.WFCount

translateAggrType :: (A.Expr -> Q.ColumnExpr) -> A.AggrType -> Q.AggregateFunction
translateAggrType translate aggr = case aggr of
    A.Avg e           -> Q.AFAvg $ translate e
    A.Max e           -> Q.AFMax $ translate e
    A.Min e           -> Q.AFMin $ translate e
    A.Sum e           -> Q.AFSum $ translate e
    A.Count e         -> Q.AFCount $ translate e
    A.CountStar       -> Q.AFCountStar
    A.CountDistinct e -> Q.AFCountDistinct $ translate e
    A.All e           -> Q.AFAll $ translate e
    A.Any e           -> Q.AFAny $ translate e

translateExprTempl :: (Maybe [Q.SelectColumn] -> A.Expr -> a)
                   -> (Q.ValueExprTemplate a -> a)
                   -> ([Q.SelectColumn] -> String -> a)
                   -> Maybe String
                   -> Maybe [Q.SelectColumn]
                   -> A.Expr
                   -> a
translateExprTempl rec wrap inline mPrefix optSelectClause expr =
    case expr of
        A.TernaryAppE A.Between c t e       ->
            wrap $ Q.VEBetween (rec optSelectClause c)
                               (rec optSelectClause t)
                               (rec optSelectClause e)
        A.TernaryAppE A.If c t e       ->
            wrap $ Q.VECase (rec optSelectClause c)
                            (rec optSelectClause t)
                            (rec optSelectClause e)

        A.BinAppE f e1 e2 ->
            wrap $ Q.VEBinApp (translateBinFun f)
                              (rec optSelectClause e1)
                              $ rec optSelectClause e2
        A.UnAppE f e      ->
            wrap $ Q.VEUnApp (translateUnFun f) (rec optSelectClause e)

        A.ColE n          -> case optSelectClause of
            Just s  -> inline s n
            Nothing -> wrap $ Q.VEColumn n mPrefix
        A.ConstE v        -> wrap $ Q.VEValue $ translateAVal v

translateExprCE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.ColumnExpr
translateExprCE = translateExprTempl translateExprCE Q.CEBase inlineCE Nothing

translateExprEE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.ExtendedExpr
translateExprEE = translateExprTempl translateExprEE Q.EEBase inlineEE Nothing

translateExprAE :: Maybe [Q.SelectColumn] -> A.Expr -> Q.AggrExpr
translateExprAE = translateExprTempl translateExprAE Q.AEBase inlineAE Nothing

--------------------------------------------------------------------------------
-- Translation of join conditions for explicit join syntax.

-- | Translate an expression that occurs in the join condition of an
-- explicit join (e.g. LEFT OUTER JOIN). No expressions are inlined
-- and all column references are prefixed with the subquery alias of
-- the respective join argument.
translateJoinExpr :: String -> A.Expr -> Q.ColumnExpr
translateJoinExpr prefix expr =
    case expr of
        A.TernaryAppE A.If c t e       ->
            Q.CEBase $ Q.VECase (translateJoinExpr prefix c)
                                (translateJoinExpr prefix t)
                                (translateJoinExpr prefix e)
        A.TernaryAppE A.Between e1 e2 e3      ->
            Q.CEBase $ Q.VECase (translateJoinExpr prefix e1)
                                (translateJoinExpr prefix e2)
                                (translateJoinExpr prefix e3)

        A.BinAppE f e1 e2 ->
            Q.CEBase $ Q.VEBinApp (translateBinFun f)
                                  (translateJoinExpr prefix e1)
                                  (translateJoinExpr prefix e2)
        A.UnAppE f e      ->
            Q.CEBase $ Q.VEUnApp (translateUnFun f) (translateJoinExpr prefix e)

        A.ColE n          -> Q.CEBase (Q.VEColumn n (Just prefix) )
        A.ConstE v        -> Q.CEBase $ Q.VEValue $ translateAVal v

translateExplJoinConds :: String
                       -> String
                       -> [(A.Expr, A.Expr, A.JoinRel)]
                       -> Q.ColumnExpr
translateExplJoinConds leftPrefix rightPrefix (jc : jcs) =
    L.foldl' (\ae e -> Q.CEBase $ Q.VEBinApp Q.BFAnd ae e)
             (translateExplJoinCond leftPrefix rightPrefix jc)
             (map (translateExplJoinCond leftPrefix rightPrefix) jcs)
translateExplJoinConds _          _           []         =
    $impossible

translateExplJoinCond :: String
                      -> String
                      -> (A.Expr, A.Expr, A.JoinRel)
                      -> Q.ColumnExpr
translateExplJoinCond leftPrefix rightPrefix (l, r, j) =
    Q.CEBase $ Q.VEBinApp (translateJoinRel j)
                          (translateJoinExpr leftPrefix l)
                          (translateJoinExpr rightPrefix r)

--------------------------------------------------------------------------------

translateUnFun :: A.UnFun -> Q.UnaryFunction
translateUnFun f = case f of
    A.Not               -> Q.UFNot
    A.Cast t            -> (Q.UFCast $ translateATy t)
    A.Sin               -> Q.UFSin
    A.Cos               -> Q.UFCos
    A.Tan               -> Q.UFTan
    A.ASin              -> Q.UFASin
    A.ACos              -> Q.UFACos
    A.ATan              -> Q.UFATan
    A.Sqrt              -> Q.UFSqrt
    A.Log               -> Q.UFLog
    A.Ln                -> Q.UFLn
    A.Exp               -> Q.UFExp
    A.SubString from to -> (Q.UFSubString from to)
    A.DateDay           -> (Q.UFExtract Q.ExtractDay)
    A.DateYear          -> (Q.UFExtract Q.ExtractYear)
    A.DateMonth         -> (Q.UFExtract Q.ExtractMonth)
    A.IsNull            -> Q.UFIsNull

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
    A.Coalesce  -> Q.BFCoalesce

-- | Translate sort information into '[Q.WindowOrderExpr]', using the column
-- function, which takes a 'String'.
translateSortInf :: [A.SortSpec]
                 -> (A.Expr -> Q.AggrExpr)
                 -> [Q.WindowOrderExpr]
translateSortInf sortInfos colFun = map toWOE sortInfos
  where
    toWOE (n, d) = Q.WOE (colFun n) (translateSortDir d)


-- | Translate a single join condition into it's 'Q.ColumnExpr' equivalent.
-- 'A.Expr' contained within the join condition are inlined with the
-- corresponding select clauses.
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
    A.VInt i       -> Q.VInteger i
    A.VStr s       -> Q.VText s
    A.VBool b      -> Q.VBoolean b
    A.VDouble d    -> Q.VDoublePrecision d
    A.VDec d       -> Q.VDecimal d
    A.VDate d      -> Q.VDate $ A.unDate d

translateATy :: A.ATy -> Q.DataType
translateATy t = case t of
    A.AInt    -> Q.DTInteger
    A.AStr    -> Q.DTText
    A.ABool   -> Q.DTBoolean
    A.ADec    -> Q.DTDecimal
    A.ADouble -> Q.DTDoublePrecision
    A.ADate   -> Q.DTDate
