module Database.Algebra.SQL.Materialization.TemporaryTable
    ( materialize
    ) where

import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Query
import Database.Algebra.SQL.Tile
import Database.Algebra.SQL.Tile.Flatten

-- | Wrap all dependencies into temporary tables, and put the root tiles into
-- value queries.
materialize :: MatFun
materialize transformResult =
    ( map tmpTable deps
    , map (QValueQuery . VQSelect . fst) selects
    )
  where
        tmpTable (name, (body, _)) =
            QDefinitionQuery $ DQTemporaryTable (VQSelect body) name

        selects :: [FlatTile String]
        deps    :: [(String, FlatTile String)]
        (selects, deps)            =
            flattenTransformResult transformResult

