-- | When the tiles have been transformed, all children of 'TileNode' are
-- references and every tree contains just a single node. This is where this
-- module is used, it removes the redundant type structure.
-- It also provides the possibilty to change the transformation behaviour, but
-- the tile merging is much worse, since we lost a lot of useful information.
--
-- This can be used as a finishing step for materialization or as an
-- intermediate step, since it provides an interface for 'String' and a
-- generic interface which takes a materializer and a substituter.
module Database.Algebra.SQL.Tile.Flatten
    ( flattenTransformResultWith
    , flattenTransformResult
    , FlatTile
    ) where

import qualified Data.IntMap.Lazy as IntMap
    ( IntMap
    , empty
    , insert
    , lookup
    )
import qualified Data.MultiSet as MultiSet
    ( MultiSet
    , empty
    , insert
    , union
    , singleton
    )
import qualified Data.DList as DL
    ( toList
    )
import Data.Maybe (fromMaybe)

import qualified Database.Algebra.SQL.Query as Q
import Database.Algebra.SQL.Query.Substitution
import Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkPCol
    )
import Database.Algebra.SQL.Tile


-- TODO error used in lookup

-- | A flat tile containing:
--
--     * the body of the tile
--
--     * a multiset of a type which identifies a referenceable flat tile
--
type FlatTile a = (Q.SelectStmt, MultiSet.MultiSet a)

-- | Flatten a transform result using SQL identifiers directly.
flattenTransformResult :: ([TileTree], DependencyList)
                       -> ([FlatTile String], [(String, FlatTile String)])
flattenTransformResult =
    flattenTransformResultWith m s
  where -- Materializes a table reference as string (a SQL identifier).
        m tableRef    = 't' : show tableRef
        s             = Q.FETableReference

-- | Flatten the transformed result and apply the given functions where
-- possible.
-- The materializer should produce the desired representation, and the
-- substituter should produce expressions which can be substituted as
-- 'Q.FromExpr'.
flattenTransformResultWith :: Ord a
                           => (ExternalReference -> a) -- ^ The materializer.
                           -> (a -> Q.FromExpr)        -- ^ The substituter.
                           -> ([TileTree], DependencyList)
                           -> ([FlatTile a], [(a, FlatTile a)])
flattenTransformResultWith materializer substituter (tiles, deps) =
    ( map (flattenTileTreeWith materializer substituter) tiles
    , map f $ DL.toList deps
    )
  where f (extRef, tile) =
            ( materializer extRef
            , flattenTileTreeWith materializer substituter tile
            )

flattenTileTreeWith :: Ord a
                    => (ExternalReference -> a)
                    -> (a -> Q.FromExpr)
                    -> TileTree
                    -> FlatTile a
flattenTileTreeWith materializer substituter (TileNode m body children) =
    flattenTileNodeWith materializer substituter m body children

-- This case should never happen, but is provided for completeness.
flattenTileTreeWith materializer substituter (ReferenceLeaf tableId s)  =
    ( emptySelectStmt
      { Q.selectClause = map f s
      , Q.fromClause = 
            [Q.FPAlias (substituter alias) aliasName $ Just s]
      }
    , MultiSet.singleton alias
    )
  where f col         = Q.SCAlias (Q.SEValueExpr $ mkPCol aliasName col) col
        aliasName     = "tmpAlias"
        alias         = materializer tableId

flattenTileNodeWith :: Ord a
                    => (ExternalReference -> a)
                    -> (a -> Q.FromExpr)
                    -> Bool
                    -> Q.SelectStmt
                    -> TileChildren
                    -> FlatTile a
flattenTileNodeWith materializer substituter mergeable body children =
    -- Merge the replacements into the body.
    ( replaceReferencesSelectStmt lookupFun body
    , externalReferences
    )
  where lookupFun v                           =
            fromMaybe (error "missing reference while replacing")
                      $ IntMap.lookup v bodySubstitutes
        (bodySubstitutes, externalReferences) =
            foldr f (IntMap.empty, MultiSet.empty) children

        f (ref, tile) (substitutes, refs) = case tile of
            -- There is probably no better way to merge here, because we have no
            -- information about where the references are located. (TODO lookup
            -- is possible, but expensive)
            TileNode m b c           ->
                let (b', externalRefs) =
                        flattenTileNodeWith materializer substituter m b c
                in
                ( IntMap.insert ref (Q.FESubQuery $ Q.VQSelect b') substitutes
                , MultiSet.union externalRefs refs
                )

            -- Most of the time this case should be taken.
            ReferenceLeaf tableId _  ->
                ( IntMap.insert ref (substituter alias) substitutes
                , MultiSet.insert alias refs
                )
              where alias = materializer tableId

