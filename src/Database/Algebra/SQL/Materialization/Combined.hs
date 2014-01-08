-- | Materializes tiles which are reachable through multiple root tiles as
-- temporary tables and everything else by using common tables expressions.
-- It is possible to choose the binding strategy for common table expressions:
--
--     * Bind in lowest possible CTE, results in toughest possible scoping,
--       tiles are only bound where they are actually used.
--
--     * Bind in highest possible CTE, tiles are bound at the highest
--       possible CTE, results in very few common table expressions.
--
module Database.Algebra.SQL.Materialization.Combined
    ( BindingStrategy(Lowest, Highest)
    , materialize
    , materializeByBindingStrategy
    ) where

import Control.Monad (when)
import Control.Monad.State.Strict
    ( State
    , gets
    , modify
    , execState
    )
import qualified Data.IntMap.Lazy as IntMap
    ( IntMap
    , alter
    , empty
    , foldrWithKey
    , insert
    , lookup
    )
import qualified Data.List as L (intersect)
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import qualified Data.Set as S (toList)

import Database.Algebra.SQL.Materialization
import qualified Database.Algebra.SQL.Materialization.Graph as G
import qualified Database.Algebra.SQL.Query as Q
    ( DefinitionQuery(DQTemporaryTable)
    , FromExpr(FEVariable, FETableReference)
    , Query(QValueQuery, QDefinitionQuery)
    , SelectStmt
    , ValueQuery(VQSelect, VQCommonTableExpression)
    )
import Database.Algebra.SQL.Query.Substitution
import Database.Algebra.SQL.Tile.Flatten

-- TODO maybe replace lists with sets? because difference should be faster
-- TODO since all root tiles are enumerated with negative numbers, the root
-- vertex check could be simply: v < 0 (O(n) -> O(1))
-- TODO in addition to the previous point add this information to the definiton
-- of transform
-- TODO maybe add schemata to the value query builder function

{-
    Definition: Single parent ancestor
        Every vertex v for which |parents(v)| <= 1 is a single parent ancestor
        for every vertex w which can only be reached through paths containing v.

        The single parent ancestors of a vertex u are called spa(u).

    Lemma:
        Let p_1, ..., p_n be parents of v, then the set of single parent
        ancestors is defined as:
            spa(v) = \bigcap_{i = 1, ..., n} spa'(p_i)

            spa'(v) = spa(v) \cup {v}

    Proof: Induction over the vertices

        Hypothesis:

                spa(v) contains all single parent ancestors of v
            <=> \forall u \in spa(v): u is in every path to v

        Basis:
            v has no parents:
                spa(v) = {}

                    
                   spa(v) = {}
                => \forall u \in spa(v): u is in every path to v

        Inductive step:
            v has parents p_1, ..., p_n:
                spa(v) = spa'(p_1) \cap spa'(p_2) \cap ... \cap spa'(p_n)
                       = (spa(p_1) \cup {p_1}) \cap ...
                                               \cap (spa(p_n) \cup {p_n})

                    u \in spa(v)
                => u \in ((spa(p_1) \cup {p_1}) \cap ...
                                                \cap (spa(p_n) \cup {p_n}))

                =>      u \in (\bigcap_{i = 1, ..., n} {p_i})
                   \lor u \in (\bigcap_{i = 1, ..., n} spa(p_i))

                \overline{Inductive hypothesis}{=>}
                    \forall u \in spa(v): u is in every path to v

                If n = 1, then the first part of the disjunction will occur,
                and/or the inductive hypothesis is used for the second part.

    Note that vertices contained in maps are always topologically sorted,
    because the transform algorithm generates those by an in-order traversal.
-}

-- | Describes the binding behaviour within a dependency tree.
data BindingStrategy = Lowest | Highest

-- | Merges all tiles reachable by a single root tile into nested common table
-- expressions (depending on their scope) and all tiles reachable by multiple
-- root tiles into a temporary table. The binding strategy determines whether it
-- is merged in the highest possible CTE or in the lowest.
materializeByBindingStrategy :: BindingStrategy -> MatFun
materializeByBindingStrategy bs =
    materializeByFunction $ case bs of
        Lowest  -> chooseLowestSPA
        Highest -> chooseHighestSPA

-- | Same as 'materializeByBehaviour' with 'Lowest' as behaviour.
materialize :: MatFun
materialize = materializeByFunction chooseLowestSPA

-- | Merges all tiles reachable by a single root tile into nested common table
-- expressions (depending on their scope) and all tiles reachable by multiple
-- root tiles into a temporary table.
materializeByFunction :: (IntMap.IntMap [G.Vertex] -> IntMap.IntMap [G.Vertex])
                      -> MatFun
materializeByFunction chooseSingleSPA transformResult =
    queriesFromSPA graph rootVertices tmpVertices reversedSpaMap
  where reversedSpaMap         = inToOutAdjMap chosenSpaMap
        chosenSpaMap           = chooseSingleSPA iSpaMap
        tmpVertices            = IntMap.foldrWithKey f [] iSpaMap
        f k l r                = case l of
            [] -> if k `elem` rootVertices
                  then r
                  else k : r
            _  -> r
        iSpaMap                = findSPA graph rootVertices
        (rootTiles, enumDeps)  = flattenTransformResultWith id
                                                            Q.FEVariable
                                                            transformResult
        graph                  = graphFromFlatResult $ enumRootTiles ++ enumDeps
        -- Enumerated root tiles.
        enumRootTiles          = zip [-1, -2 ..] rootTiles
        rootVertices           = map fst enumRootTiles

-- | The used graph.
type Graph = G.Graph Q.SelectStmt

graphFromFlatResult :: [(Int, FlatTile Int)]
                    -> Graph
graphFromFlatResult enumTiles =
    G.mkGraph $ map f enumTiles
  where f (identifier, (t, ds)) = (t, identifier, S.toList ds)

-- | The lowest single parent ancestor state contains:
--     * A map of vertices mapping to their single parent ancestors.
--
type SState = IntMap.IntMap [G.Vertex]

-- | The state monad used to find single parent ancestors.
type SFinder = State SState

-- | Returns the list of single parent ancestors for a vertex or the empty list
-- if the vertex has not been processed yet.
sfGetSingleParentAncestors :: G.Vertex -> SFinder [G.Vertex]
sfGetSingleParentAncestors v = do
    result <- gets $ IntMap.lookup v

    return $ case result of
                 -- Already calculated, return the spas and v itself.
                 Just spas -> v : spas
                 -- No entry yet. (Won't be called.)
                 Nothing   -> []

-- | Take a list of vertices and intersect their single parent ancestors with
-- each other, effectively calculating the single parent ancestors for this
-- vertex.
sfComputeSingleParentAncestors :: G.Vertex -> [G.Vertex] -> SFinder ()
sfComputeSingleParentAncestors v (pv:pvs) = do
    spa <- sfGetSingleParentAncestors pv
    spas <- mapM sfGetSingleParentAncestors pvs

    modify $ IntMap.insert v $ foldr L.intersect spa spas

sfComputeSingleParentAncestors v []       =
    -- v is a top level vertex.
    modify $ IntMap.insert v []

sfVertexProcessed :: G.Vertex -> SFinder Bool
sfVertexProcessed v = gets $ isJust . IntMap.lookup v

traverse :: Graph    -- ^ The used graph.
         -> G.Vertex -- ^ The current vertex.
         -> SFinder ()
traverse graph v = do
    processedList <- mapM sfVertexProcessed parents

    -- Check whether all parents have been processed.
    when (and processedList) $ do
        sfComputeSingleParentAncestors v parents
        
        -- Recurse over its children.
        mapM_ (traverse graph) $ G.children v graph

  where parents = G.parents v graph

-- | This function descends the given root vertices and returns the single
-- parent ancestors for each vertex, reachable by any of the given root
-- vertices.
-- A vertex with parents, which are not reachable through the given root nodes
-- can and will not be computed.
findSPA :: Graph
        -> [G.Vertex]
        -> IntMap.IntMap [G.Vertex]
findSPA graph rootVertices =
    -- Collect the results with the SFinder MonadState.
    execState (mapM_ (traverse graph) rootVertices) IntMap.empty
            
-- | Chooses the lowest single parent ancestor for each vertex.
chooseLowestSPA :: IntMap.IntMap [G.Vertex] -> IntMap.IntMap [G.Vertex]
chooseLowestSPA = fmap $ take 1

-- | Chooses the highest single parent ancestor for each vertex.
chooseHighestSPA :: IntMap.IntMap [G.Vertex] -> IntMap.IntMap [G.Vertex]
chooseHighestSPA = fmap f
  where f l = case l of
            [] -> []
            _  -> [last l]

-- | Reverses an out-adjacency map into an in-adjacency map.
inToOutAdjMap :: IntMap.IntMap [G.Vertex] -> IntMap.IntMap [G.Vertex]
inToOutAdjMap = IntMap.foldrWithKey f IntMap.empty
  where f key vertices rMap = foldr (g key) rMap vertices
        g key               = IntMap.alter (h key)
        h v (Just x)        = Just $ v : x
        h v Nothing         = Just [v]

-- | Constructs a list of queries from the given arguments.
-- Takes an out-adjacency map of the single parent ancestors (which means the
-- child vertices are mapped from their corresponding single parent ancestor).
-- The map should resemble a tree structure (i.e. no vertex has multiple
-- parents), otherwise queries are executed multiple times.
queriesFromSPA :: Graph                    -- ^ Labeled graph.
               -> [G.Vertex]               -- ^ Root vertices.
               -> [G.Vertex]               -- ^ Temporary vertices.
               -> IntMap.IntMap [G.Vertex] -- ^ Out adjacency list.
               -> ([Q.Query], [Q.Query])
queriesFromSPA graph rootVertices tmpVertices reversedSpaMap =
    (tmpQueries, rootQueries)
  where tmpQueries  = map tmpFun tmpVertices
        tmpFun v    = Q.QDefinitionQuery . Q.DQTemporaryTable (build v)
                                                              $ mat v
        rootQueries = map (Q.QValueQuery . build) rootVertices
        -- The materializer: t0, t1, t2, ...
        mat vertex  = 't' : show vertex
        build       = buildValueQuery graph reversedSpaMap mat

-- | Traverses the reversed SPA map like a tree, building a tree of CTEs,
-- starting at the given vertex.
buildValueQuery :: Graph                    -- ^ The corresponding graph.
                -> IntMap.IntMap [G.Vertex] -- ^ The reversed spa map.
                -> (G.Vertex -> String)     -- ^ The materializer.
                -> G.Vertex                 -- ^ Vertex to build the query for.
                -> Q.ValueQuery
buildValueQuery graph reversedSpaMap mat v =
    if null bindings
    then body
    else Q.VQCommonTableExpression body bindings
  where childVertices = fromMaybe [] $ IntMap.lookup v reversedSpaMap
        childQueries  = map (buildValueQuery graph reversedSpaMap mat)
                            childVertices
        bindings      = zip3 (map mat childVertices)
                             (repeat Nothing)
                             childQueries
        body          = Q.VQSelect
                        $ replaceReferencesSelectStmt (Q.FETableReference . mat)
                                                      select
        select        = fromMaybe (error "missing node label") $ G.node v graph

