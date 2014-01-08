-- | This module abstracts over commonly used functions.
module Database.Algebra.SQL.Util
    ( renderOutput
    , renderDebugOutput
    , renderOutputDSH
    , putShowSLn
    ) where

import Data.List (intersperse)

import qualified Database.Algebra.SQL.Render as R
import qualified Database.Algebra.SQL.Tile as T
import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Materialization.Combined as C
import Database.Algebra.SQL.Query (Query)

resultFromDAG :: T.PFDag -> MatFun -> (T.TransformResult, ([Query], [Query]))
resultFromDAG dag matFun = (transformResult, matFun transformResult)
  where transformResult = T.transform dag

-- | Produces pretty output, optionally with debug information.
renderDebugOutput :: T.PFDag -> MatFun -> Bool -> ShowS
renderDebugOutput dag matFun debug =
    ( if debug
      then dBegin . R.debugTransformResult r . showChar '\n'
      else id
    )
    . begin
    . foldr (.) id (intersperse mid $ R.renderPretty $ tqs ++ rqs)
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

-- | Renders a DAG in an ugly but fast way, feasible for direct SQL input.
renderOutput :: T.PFDag -> MatFun -> ShowS
renderOutput dag matFun = foldr (.) id $ intersperse (showChar '\n') renderedQs
  where (_, (tqs, rqs)) = resultFromDAG dag matFun
        renderedQs      = R.render $ tqs ++ rqs

-- | Render output directly for DSH. The order from the root nodes in the
-- directed acyclic graph is preserved.
renderOutputDSH :: T.PFDag -> (String, [String])
renderOutputDSH dag =
    ( foldr (.) id renderedTQs ""
    , map ($ "") renderedRQs
    )
  where
    (_, (tqs, rqs)) = resultFromDAG dag C.materialize
    renderedRQs     = R.render rqs
    renderedTQs     = R.render tqs
