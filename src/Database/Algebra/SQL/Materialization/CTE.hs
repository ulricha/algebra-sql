module Database.Algebra.SQL.Materialization.CTE
    ( materialize
    ) where

import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Query
import Database.Algebra.SQL.Tile.Flatten

-- | Create a CTE for every root tile and add a binding for every dependency.
materialize:: MatFun
materialize transformResult =
    ( []
    , if null bindings
      then map (QValueQuery . VQSelect . fst) selects
      else map
           (QValueQuery . flip VQCommonTableExpression bindings . VQSelect . fst)
           selects
    )
  where bindings            = map f deps
        f (name, (body, _)) = (name, Nothing, VQSelect body)

        selects :: [FlatTile String]
        deps    :: [(String, FlatTile String)]
        (selects, deps)     =
            flattenTransformResult transformResult

