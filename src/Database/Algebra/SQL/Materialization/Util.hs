
module Database.Algebra.SQL.Materialization.Util
    ( Graph
    , graphFromFlatResult
    ) where

import qualified Data.MultiSet as MS

import Database.Algebra.SQL.Tile.Flatten
import qualified Database.Algebra.SQL.Query as Q
import qualified Database.Algebra.SQL.Materialization.Graph as G

-- | The used graph.
type Graph = G.Graph Q.SelectStmt

-- | Generate a graph from tiles.
graphFromFlatResult :: [(Int, FlatTile Int)]
                    -> Graph
graphFromFlatResult enumTiles =
    G.mkGraph $ map f enumTiles
  where f (identifier, (t, ds)) = (t, identifier, MS.toList ds)


