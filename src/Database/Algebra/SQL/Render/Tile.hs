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
import Database.Algebra.SQL.Compatibility

intRef :: InternalReference -> Doc
intRef = ondullblue . int

extRef :: ExternalReference -> Doc
extRef = onwhite . black . bold . int

type_ :: String -> Doc
type_ = bold . text

renderTileTreeNode :: CompatMode -> Q.SelectStmt ->  [(Int, TileTree)] -> Doc
renderTileTreeNode compat body children =
    type_ "tile"
    <$> RQ.renderSelectStmt compat body
    <> if null children
       then empty
       else linebreak <> bold (text "children") <+> align (vsep rC)
  where rC         = map f children
        f (vId, t) = intRef vId <+> align (renderTileTree compat t)


renderTileTree :: CompatMode -> TileTree -> Doc
renderTileTree _ (ReferenceLeaf n _)                = type_ "references"
                                                    <+> extRef n
renderTileTree compat (TileNode features body children) =
    type_ ( show features
          )
    <+> renderTileTreeNode compat body children

renderTransformResult :: CompatMode -> ([TileTree], DependencyList) -> Doc
renderTransformResult compat (ts, dl) =
    type_ "queries"
    <$> indent 4 (vcat (punctuate linebreak (map (renderTileTree compat) ts)))
    <> if null depList
       then empty
       else ( linebreak
              <> type_ "dependencies"
              <$> indent 4 deps
            )
  where deps        = vsep $ map f depList
        f (r, tree) = extRef r
                      <+> bold (text "as")
                      <+> align (renderTileTree compat tree)
        depList     = DL.toList dl
