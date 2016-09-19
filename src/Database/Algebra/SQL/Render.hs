-- | This mdoule provides efficient functions to render lists of queries.
module Database.Algebra.SQL.Render
    ( debugTransformResult
    , renderCompact
    , renderPretty
    , renderPlain
    ) where

import qualified Text.PrettyPrint.ANSI.Leijen      as L (Doc, SimpleDoc,
                                                         displayS, plain,
                                                         renderCompact,
                                                         renderPretty)

import           Database.Algebra.SQL.Dialect
import           Database.Algebra.SQL.Query        (Query)
import           Database.Algebra.SQL.Render.Query (renderQuery)
import           Database.Algebra.SQL.Render.Tile  (renderTransformResult)
import           Database.Algebra.SQL.Tile         (TileDep, TileTree)

renderPrettySimpleDoc :: L.Doc -> L.SimpleDoc
renderPrettySimpleDoc =
    L.renderPretty 0.8 80

renderWith :: (L.Doc -> L.SimpleDoc) -> Dialect -> Query -> ShowS
renderWith f c = L.displayS . f . renderQuery c


-- | Returns a 'ShowS' containing debug information for a transform result.
debugTransformResult :: Dialect -> ([TileTree], [TileDep]) -> ShowS
debugTransformResult compat =
    L.displayS . renderPrettySimpleDoc . (renderTransformResult compat)

-- | Renders a list of queries in an ugly but fast way, feasible as direct SQL
-- input.
renderCompact :: Dialect -> [Query] -> [ShowS]
renderCompact c = map $ renderWith L.renderCompact c

-- | Renders a list of queries in a beautiful way.
renderPretty :: Dialect -> [Query] -> [ShowS]
renderPretty c = map $ renderWith renderPrettySimpleDoc c

-- | Renders a list of queries without colors but formatted.
renderPlain :: Dialect -> [Query] -> [ShowS]
renderPlain c = map $ renderWith (renderPrettySimpleDoc . L.plain) c

