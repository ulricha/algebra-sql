-- | This mdoule provides efficient functions to render lists of queries.
module Database.Algebra.SQL.Render
    ( debugTransformResult
    , render
    , renderPretty
    ) where

import Data.List (intercalate)
import qualified Text.PrettyPrint.ANSI.Leijen as L
    ( Doc
    , SimpleDoc
    , displayS
    , renderPretty
    , renderCompact
    )

import Database.Algebra.SQL.Query (Query)
import Database.Algebra.SQL.Render.Query (renderQuery)
import Database.Algebra.SQL.Render.Tile (renderTransformResult)
import Database.Algebra.SQL.Tile (TransformResult)

renderPrettySimpleDoc :: L.Doc -> L.SimpleDoc
renderPrettySimpleDoc =
    L.renderPretty 0.8 80

renderWith :: (L.Doc -> L.SimpleDoc) -> Query -> ShowS
renderWith f = L.displayS . f . renderQuery


-- | Returns a 'ShowS' containing debug information for a transform result.
debugTransformResult :: TransformResult -> ShowS
debugTransformResult =
    L.displayS . renderPrettySimpleDoc . renderTransformResult

-- | Renders a list of queries in an ugly but fast way, feasible as direct SQL
-- input.
render :: [Query] -> [ShowS]
render = map (renderWith L.renderCompact)

-- | Renders a list of queries in a beautiful way.
renderPretty :: [Query] -> [ShowS]
renderPretty = map (renderWith renderPrettySimpleDoc)

