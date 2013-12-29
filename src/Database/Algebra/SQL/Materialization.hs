-- Provides a basic toolset for materialization functions.
module Database.Algebra.SQL.Materialization where

import Database.Algebra.SQL.Tile (TileTree, DependencyList)
import Database.Algebra.SQL.Query (Query)

-- The type of materialization function.
type MatFun = (([TileTree], DependencyList) -> [Query])

