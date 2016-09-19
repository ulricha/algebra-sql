-- | This module abstracts over commonly used functions.
module Database.Algebra.SQL.Util
    ( renderOutputCompact
    , renderOutputPlain
    , renderDebugOutput
    , renderAdvancedDebugOutput
    , renderOutputDSH
    , renderOutputDSHWith
    , putShowSLn
    ) where

import Data.List (intersperse)

import qualified Database.Algebra.SQL.Render as R
import qualified Database.Algebra.SQL.Tile as T
import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Materialization.Combined as C
import Database.Algebra.SQL.Query (Query)
import Database.Algebra.SQL.Dialect

-- TODO include materialization strategy depending on compat mode

resultFromDAG :: T.TADag -> MatFun -> (([T.TileTree], [T.TileDep]), ([Query], [Query]))
resultFromDAG dag matFun = (tiles, matFun tiles)
  where
    tiles = T.tilePlan dag

-- | Produces pretty output, optionally with debug information.
renderDebugOutput :: Dialect -> T.TADag -> MatFun -> Bool -> ShowS
renderDebugOutput c dag matFun debug =
    ( if debug
      then dBegin . R.debugTransformResult c r . showChar '\n'
      else id
    )
    . begin
    . foldr (.) id (intersperse mid $ R.renderPretty c $ tqs ++ rqs)
    . end

  where -- SQL compliant separators. (comments)
        dBegin  = showString "----- debug output: tile\n"
        begin   = showString "----- graph output begin   -->\n"
        end     = showString "\n----- graph output end     <--\n"
        mid     = showString "\n----- additional query\n"
        (r, (tqs, rqs))
                = resultFromDAG dag matFun

putShowSLn :: ShowS -> IO ()
putShowSLn s = putStrLn $ s ""

-- | Renders a DAG with the given renderer.
renderOutputWith :: (Dialect -> [Query] -> [ShowS])
                 -> Dialect
                 -> T.TADag
                 -> MatFun
                 -> ShowS
renderOutputWith renderer c dag matFun =
    foldr (.) id $ intersperse (showChar '\n') renderedQs
  where (_, (tqs, rqs)) = resultFromDAG dag matFun
        renderedQs      = renderer c $ tqs ++ rqs

renderOutputCompact :: Dialect -> T.TADag -> MatFun -> ShowS
renderOutputCompact = renderOutputWith R.renderCompact

renderOutputPlain :: Dialect -> T.TADag -> MatFun -> ShowS
renderOutputPlain = renderOutputWith R.renderPlain

-- | Render output directly for DSH. The order from the root nodes in the
-- directed acyclic graph is preserved. (This function uses the combined
-- materialization strategy.)
renderOutputDSH :: Dialect -> T.TADag -> (Maybe String, [String])
renderOutputDSH = flip renderOutputDSHWith C.materialize

-- | Render output directly for DSH. The order from the root nodes in the
-- directed acyclic graph is preserved.
renderOutputDSHWith :: Dialect -> MatFun -> T.TADag -> (Maybe String, [String])
renderOutputDSHWith c matFun dag =
    ( if null preludeString
      then Nothing
      else Just preludeString
    , map ($ "") renderedRQs
    )
  where
    preludeString   = foldr (.) id renderedTQs ""
    (_, (tqs, rqs)) = resultFromDAG dag matFun
    renderedRQs     = R.renderPlain c rqs
    renderedTQs     = R.renderPlain c tqs

-- | Produces output which allows further inspection with the psql command line
-- utility (and possibly others too).
renderAdvancedDebugOutput :: Dialect -> Bool -> Bool -> T.TADag -> MatFun -> String
renderAdvancedDebugOutput c explain analyze dag matFun =
    foldr ((.) . (prefixes .)) id (renderedTQs ++ renderedRQs) ""
  where
    (_, (tqs, rqs)) = resultFromDAG dag matFun
    renderedRQs     = R.renderPlain c rqs
    renderedTQs     = R.renderPlain c tqs
    prefixes        = showString $ ep ++ ap
    ep              = if explain then "EXPLAIN " else ""
    ap              = if analyze then "ANALYZE " else ""
