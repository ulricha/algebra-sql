module Database.Algebra.SQL.Render.Tile
    ( renderTransformResult
    ) where


import Text.PrettyPrint.ANSI.Leijen ( (<$>)
                                    , (<+>)
                                    , (<>)
                                    , Doc
                                    , align 
                                    , black
                                    , bold
                                    , empty
                                    , indent
                                    , int
                                    , linebreak
                                    , ondullblue
                                    , onwhite
                                    , punctuate
                                    , text
                                    , vcat
                                    , vsep
                                    )
import qualified Data.DList as DL
    ( toList
    )

import qualified Database.Algebra.SQL.Render.Query as RQ
import qualified Database.Algebra.SQL.Query as Q
import Database.Algebra.SQL.Tile

intRef :: InternalReference -> Doc
intRef = ondullblue . int

extRef :: ExternalReference -> Doc
extRef = onwhite . black . bold . int

type_ :: String -> Doc
type_ = bold . text

renderTileTreeNode :: Q.SelectStmt ->  [(Int, TileTree)] -> Doc
renderTileTreeNode body children =
    type_ "tile"
    <$> RQ.renderSelectStmt body
    <> if null children
       then empty
       else linebreak <> bold (text "children") <+> align (vsep rC)
  where rC         = map f children
        f (vId, t) = intRef vId <+> align (renderTileTree t)


renderTileTree :: TileTree -> Doc
renderTileTree (ReferenceLeaf n _)                = type_ "references"
                                                    <+> extRef n
renderTileTree (TileNode mergeable body children) =
    type_ ( case mergeable of
                True  -> "open"
                False -> "closed"
          )
    <+> renderTileTreeNode body children

renderTransformResult :: ([TileTree], DependencyList) -> Doc
renderTransformResult (ts, dl) =
    type_ "queries"
    <$> indent 4 (vcat (punctuate linebreak (map renderTileTree ts)))
    <> if null depList
       then empty
       else ( linebreak
              <> type_ "dependencies"
              <$> indent 4 deps
            )
  where deps        = vsep $ map f depList
        f (r, tree) = extRef r
                      <+> bold (text "as")
                      <+> align (renderTileTree tree)
        depList     = DL.toList dl
