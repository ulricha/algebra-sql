{-# LANGUAGE DoAndIfThenElse #-}
module Database.Algebra.SQL.Materialization.CTE
    ( materialize
    , legacyMaterialize
    ) where

import Control.Monad (filterM)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import Data.Maybe

import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Materialization.Util
import qualified Database.Algebra.SQL.Materialization.Graph as G
import Database.Algebra.SQL.Query
import Database.Algebra.SQL.Tile.Flatten
import Database.Algebra.SQL.Query.Substitution

-- TODO remove after testing
-- | Create a CTE for every root tile and add a binding for every dependency.
legacyMaterialize :: MatFun
legacyMaterialize transformResult =
    ( []
    , if null bindings
      then map (QValueQuery . VQSelect . fst) selects
      else map
           (QValueQuery . VQWith bindings . VQSelect . fst)
           selects
    )
  where bindings            = map f deps
        f (name, (body, _)) = (name, Nothing, VQSelect body)

        selects :: [FlatTile String]
        deps    :: [(String, FlatTile String)]
        (selects, deps)     =
            flattenTransformResult transformResult


type Gather = StateT (IntMap.IntMap SelectStmt) (Reader IntSet.IntSet)

materialize :: MatFun
materialize transformResult =
    ( []
    , map (QValueQuery . gather) rootVertices
    )
  where
    (rootTiles, enumDeps)  = flattenTransformResultWith id
                                                        FEVariable
                                                        transformResult
    graph                  = graphFromFlatResult $ enumRootTiles ++ enumDeps
    -- Enumerated root tiles.
    enumRootTiles          = zip [-1, -2 ..] rootTiles
    rootVertices           = map fst enumRootTiles


    -- Create a WITH query for a root vertex.
    gather :: G.Vertex -> ValueQuery
    gather v = case mSelect of
        Just (_, s) ->
            if IntMap.null bindings
            then VQSelect s
            else VQWith withQueryBindings $ VQSelect s
                                         
        Nothing -> error "gather: v is not a root vertex"
      where
        withQueryBindings = IntMap.foldrWithKey toBinding [] bindings

        (mSelect, bindings) = runReader (runStateT (visit v) IntMap.empty)
                                        $ IntSet.fromList $ G.reachable v graph
        
        -- TODO factor out
        toBinding ref select l = ('t' : show ref, Nothing, VQSelect select) : l

    -- The return value indicates whether the parent should inline.
    visit :: G.Vertex -> Gather (Maybe (Int, SelectStmt))
    visit v = do

        alreadyBound <- hasBinding v
        
        if alreadyBound
        -- Binding already added.
        then return Nothing
        else do

            doInline <- shouldInline v

            allResults <- mapM visit $ G.children v graph

            let -- Get those results which need inlining.
                results    = catMaybes allResults
                -- Inline those results into the current label.
                selectStmt =
                    replaceReferencesSelectStmt
                    replace
                    (fromMaybe errorMsg $ G.node v graph)
                -- The lookup function used for substitution.
                replace ref = case lookup ref results of
                    -- It will be available from the bindings of the with query.
                    Nothing -> FETableReference $ 't' : show ref
                    -- We will provide it inline.
                    Just s  -> FESubQuery $ VQSelect s

                errorMsg = error "visit: invalid vertex, no label found"

            if doInline
            then -- Inline this vertex within one or zero parents.
                return $ Just (v, selectStmt)
            else do
                -- This vertex is referenced by multiple ones, there is no way
                -- we could inline it.
                addBinding v selectStmt

                return Nothing
                
    addBinding :: G.Vertex -> SelectStmt -> Gather ()
    addBinding v selectStmt =
        modify $ IntMap.insert v selectStmt

    hasBinding :: G.Vertex -> Gather Bool
    hasBinding v = gets $ IntMap.member v

    reachableByRoot :: G.Vertex -> Gather Bool
    reachableByRoot v = asks $ IntSet.member v

    shouldInline :: G.Vertex -> Gather Bool
    shouldInline v = do

        knownParents <- filterM reachableByRoot $ G.parents v graph

        return $ case knownParents of
            []  -> True
            -- Has just one known parent.
            [p] -> True
            _   -> False
