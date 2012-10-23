module Database.Algebra.VL.Render.Dot(renderVLDot, renderTblVal) where
    
import qualified Data.Map as Map
import Data.List

import Text.PrettyPrint
  
import Database.Algebra.VL.Data
import Database.Algebra.Dag.Common as C
import qualified Database.Algebra.Dag as Dag

nodeToDoc :: AlgNode -> Doc
nodeToDoc n = (text "id:") <+> (int n)

tagsToDoc :: [Tag] -> Doc
tagsToDoc ts = vcat $ map text ts

labelToDoc :: AlgNode -> String -> Doc -> [Tag] -> Doc
labelToDoc n s as ts = (nodeToDoc n) $$ ((text s) <> (parens as)) $$ (tagsToDoc $ nub ts)

lookupTags :: AlgNode -> NodeMap [Tag] -> [Tag]
lookupTags n m = Map.findWithDefault [] n m

renderColumnType :: VLType -> Doc
renderColumnType = text . show

renderData :: [[VLVal]] -> Doc
renderData [] = empty
renderData xs = (flip (<>) semi . sep . punctuate semi . map renderRow) xs

renderRow :: [VLVal] -> Doc
renderRow = hcat . punctuate comma . map renderTblVal

renderTblVal :: VLVal -> Doc
renderTblVal (VLInt i) = integer $ fromIntegral i
renderTblVal (VLNat i) = text "#" <> (integer $ fromIntegral i)
renderTblVal (VLBool b) = text $ show b
renderTblVal (VLString s) = doubleQuotes $ text $ escape s
renderTblVal (VLDouble d) = double d
renderTblVal VLUnit = text "()"

escape :: String -> String
escape (x@'\\':xs) = '\\':'\\':'\\':x:escape xs
escape (x@'\'':xs) = '\\':x:escape xs
escape (x@'"':xs) = '\\':'\\':x:escape xs
escape (x:xs) = x:escape xs
escape [] = []

bracketList :: (a -> Doc) -> [a] -> Doc
bracketList f = brackets . hsep . punctuate comma . map f

renderTableType :: TypedColumn -> Doc
renderTableType (c, t) = text c <> text "::" <> renderColumnType t

renderTableKeys :: [Key] -> Doc
renderTableKeys [x] = renderTableKey x
renderTableKeys (x:xs) = renderTableKey x $$ renderTableKeys xs
renderTableKeys [] = text "NOKEY"

renderTableKey :: Key -> Doc
renderTableKey [x] = text x
renderTableKey (x:xs) = text x <> comma <+> renderTableKey xs
renderTableKey [] = text "NOKEY"
                    
renderPayloadProj :: (Doc, PayloadProj) -> Doc
renderPayloadProj (d, PLConst v) = d <> colon <> renderTblVal v
renderPayloadProj (d, PLCol c) = d <> colon <> int c

renderISTransProj :: (Doc, ISTransProj) -> Doc
renderISTransProj (d, STDescrCol) = d <> colon <> text "descr"
renderISTransProj (d, STPosCol)   = d <> colon <> text "pos"
renderISTransProj (d, STNumber)   = d <> colon <> text "#"

renderDescrProj :: (Doc, DescrProj) -> Doc
renderDescrProj (d, DescrConst v)  = d <> colon <> (integer $ toInteger v)
renderDescrProj (d, DescrIdentity) = d <> colon <> text "descr"
renderDescrProj (d, DescrPosCol)   = d <> colon <> text "pos"
                                     
renderPosProj :: (Doc, PosProj) -> Doc
renderPosProj (d, PosNumber) = d <> colon <> text "#"
renderPosProj (d, PosConst v) = d <> colon <> (integer $ toInteger v)
renderPosProj (d, PosIdentity) = d <> colon <> text "pos"
                                 
renderExpr1 :: Expr1 -> Doc
renderExpr1 (App1 op e1 e2) = (parens $ renderExpr1 e1) <+> (text $ show op) <+> (parens $ renderExpr1 e2)
renderExpr1 (Constant1 val) = renderTblVal val
renderExpr1 (Column1 c)     = text "col" <> int c

renderExpr2 :: Expr2 -> Doc
renderExpr2 (App2 op e1 e2)      = (parens $ renderExpr2 e1) <+> (text $ show op) <+> (parens $ renderExpr2 e2)
renderExpr2 (Constant2 val)      = renderTblVal val
renderExpr2 (Column2Left (L c))  = text "lcol" <> int c
renderExpr2 (Column2Right (R c)) = text "rcol" <> int c

-- create the node label from an operator description
opDotLabel :: NodeMap [Tag] -> AlgNode -> VL -> Doc
opDotLabel tm i (NullaryOp (SingletonDescr)) = labelToDoc i "SingletonDescr" empty (lookupTags i tm)
opDotLabel tm i (NullaryOp (ConstructLiteralValue tys vals)) = labelToDoc i "ConstructLiteralValue"
    (bracketList (\t -> renderColumnType t <> text "\n") tys <> comma
    $$ renderData [vals]) (lookupTags i tm)
opDotLabel tm i (NullaryOp (ConstructLiteralTable tys vals)) = labelToDoc i "ConstructLiteralTable"
        (bracketList renderColumnType tys <> comma
        $$ renderData vals) (lookupTags i tm)
opDotLabel tm i (NullaryOp (TableRef n tys ks)) = labelToDoc i "TableRef"
        (quotes (text n) <> comma <+> bracketList (\t -> renderTableType t <> text "\n") tys <> comma $$ renderTableKeys ks)
        (lookupTags i tm)
opDotLabel tm i (UnOp Unique _) = labelToDoc i "Unique" empty (lookupTags i tm)
opDotLabel tm i (UnOp UniqueL _) = labelToDoc i "UniqueL" empty (lookupTags i tm)
opDotLabel tm i (UnOp NotPrim _) = labelToDoc i "NotPrim" empty (lookupTags i tm)
opDotLabel tm i (UnOp NotVec _) = labelToDoc i "NotVec" empty (lookupTags i tm)
opDotLabel tm i (UnOp LengthA _) = labelToDoc i "LengthA" empty (lookupTags i tm)
opDotLabel tm i (UnOp DescToRename _) = labelToDoc i "DescToRename" empty (lookupTags i tm)
opDotLabel tm i (UnOp ToDescr _) = labelToDoc i "ToDescr" empty (lookupTags i tm)
opDotLabel tm i (UnOp Segment _) = labelToDoc i "Segment" empty (lookupTags i tm)
opDotLabel tm i (UnOp Unsegment _) = labelToDoc i "Unsegment" empty (lookupTags i tm)
opDotLabel tm i (UnOp (VecSum t) _) = labelToDoc i "VecSum" (renderColumnType t) (lookupTags i tm)
opDotLabel tm i (UnOp VecMin _) = labelToDoc i "VecMin" empty (lookupTags i tm)
opDotLabel tm i (UnOp VecMinL _) = labelToDoc i "VecMinL" empty (lookupTags i tm)
opDotLabel tm i (UnOp VecMax _) = labelToDoc i "VecMax" empty (lookupTags i tm)
opDotLabel tm i (UnOp VecMaxL _) = labelToDoc i "VecMaxL" empty (lookupTags i tm)
opDotLabel tm i (UnOp (ProjectL cols) _) = labelToDoc i "ProjectL" (bracketList (text . show) cols) (lookupTags i tm)
opDotLabel tm i (UnOp (ProjectA cols) _) = labelToDoc i "ProjectA" (bracketList (text . show) cols) (lookupTags i tm)
opDotLabel tm i (UnOp IntegerToDoubleA _) = labelToDoc i "IntegerToDoubleA" empty (lookupTags i tm)
opDotLabel tm i (UnOp IntegerToDoubleL _) = labelToDoc i "IntegerToDoubleL" empty (lookupTags i tm)
opDotLabel tm i (UnOp ReverseA _) = labelToDoc i "ReverseA" empty (lookupTags i tm)
opDotLabel tm i (UnOp ReverseL _) = labelToDoc i "ReverseL" empty (lookupTags i tm)
opDotLabel tm i (UnOp FalsePositions _) = labelToDoc i "FalsePositions" empty (lookupTags i tm)
opDotLabel tm i (UnOp R1 _) = labelToDoc i "R1" empty (lookupTags i tm)
opDotLabel tm i (UnOp R2 _) = labelToDoc i "R2" empty (lookupTags i tm)
opDotLabel tm i (UnOp R3 _) = labelToDoc i "R3" empty (lookupTags i tm)
opDotLabel tm i (UnOp (ProjectRename (p1, p2)) _) =
  labelToDoc i "ProjectRename" pLabel (lookupTags i tm)
  where pLabel = parens $ (renderISTransProj (text "posnew", p1)) <> comma <+> (renderISTransProj (text "posold", p2))
opDotLabel tm i (UnOp (ProjectPayload pCols) _) =
  labelToDoc i "ProjectPayload" pLabel (lookupTags i tm)
  where pLabel = valCols
        valCols = bracketList (\(j, p) -> renderPayloadProj (itemLabel j, p)) $ zip ([1..] :: [Int]) pCols
        itemLabel j = (text "item") <> (int j)
opDotLabel tm i (UnOp (ProjectAdmin (pDescr, pPos)) _) =
  labelToDoc i "ProjectAdmin" pLabel (lookupTags i tm)
  where pLabel = parens $ (renderDescrProj (text "descr", pDescr))
                 <> comma
                 <+> (renderPosProj (text "pos", pPos))
opDotLabel tm i (UnOp (SelectExpr e) _) = labelToDoc i "SelectExpr" (renderExpr1 e) (lookupTags i tm)
opDotLabel tm i (UnOp Only _) = labelToDoc i "Only" empty (lookupTags i tm)
opDotLabel tm i (UnOp (CompExpr1L expr) _) =
  labelToDoc i "CompExpr1L" (renderExpr1 expr) (lookupTags i tm)
opDotLabel tm i (UnOp Singleton _) = labelToDoc i "Singleton" empty (lookupTags i tm)
opDotLabel tm i (UnOp (SelectPos1 o (N p)) _)  = labelToDoc i "SelectPos1" ((text $ show o) <+> int p) (lookupTags i tm)
opDotLabel tm i (UnOp (SelectPos1L o (N p)) _) = labelToDoc i "SelectPos1L" ((text $ show o) <+> int p) (lookupTags i tm)
opDotLabel tm i (BinOp GroupBy _ _) = labelToDoc i "GroupBy" empty (lookupTags i tm)
opDotLabel tm i (BinOp SortWith _ _) = labelToDoc i "SortWith" empty (lookupTags i tm)
opDotLabel tm i (BinOp LengthSeg _ _) = labelToDoc i "LengthSeg" empty (lookupTags i tm)
opDotLabel tm i (BinOp DistPrim _ _) = labelToDoc i "DistPrim" empty (lookupTags i tm)
opDotLabel tm i (BinOp DistDesc _ _) = labelToDoc i "DistDesc" empty (lookupTags i tm)
opDotLabel tm i (BinOp DistLift _ _) = labelToDoc i "DistLift" empty (lookupTags i tm)
opDotLabel tm i (BinOp PropRename _ _) = labelToDoc i "PropRename" empty (lookupTags i tm)
opDotLabel tm i (BinOp PropFilter _ _) = labelToDoc i "PropFilter" empty (lookupTags i tm)
opDotLabel tm i (BinOp PropReorder _ _) = labelToDoc i "PropReorder" empty (lookupTags i tm)
opDotLabel tm i (BinOp Append _ _) = labelToDoc i "Append" empty (lookupTags i tm)
opDotLabel tm i (BinOp RestrictVec _ _) = labelToDoc i "RestrictVec" empty (lookupTags i tm)
opDotLabel tm i (BinOp (CompExpr2 expr) _ _) = labelToDoc i "CompExpr2" (renderExpr2 expr) (lookupTags i tm)
opDotLabel tm i (BinOp (CompExpr2L expr) _ _) = labelToDoc i "CompExpr2L" (renderExpr2 expr) (lookupTags i tm)
opDotLabel tm i (BinOp VecSumL _ _) = labelToDoc i "VecSumL" empty (lookupTags i tm)
opDotLabel tm i (BinOp (SelectPos o) _ _) = labelToDoc i "SelectPos" (text $ show o) (lookupTags i tm)
opDotLabel tm i (BinOp (SelectPosL o) _ _) = labelToDoc i "SelectPosL" (text $ show o) (lookupTags i tm)
opDotLabel tm i (BinOp PairA _ _) = labelToDoc i "PairA" empty (lookupTags i tm)
opDotLabel tm i (BinOp PairL _ _) = labelToDoc i "PairL" empty (lookupTags i tm)
opDotLabel tm i (BinOp ZipL _ _) = labelToDoc i "ZipL" empty (lookupTags i tm)
opDotLabel tm i (BinOp CartProduct _ _) = labelToDoc i "CartProduct" empty (lookupTags i tm)
opDotLabel tm i (BinOp (ThetaJoinFlat expr) _ _) =
  labelToDoc i "ThetaJoinFlat" (renderExpr1 expr) (lookupTags i tm)
opDotLabel tm i (TerOp CombineVec _ _ _) = labelToDoc i "CombineVec" empty (lookupTags i tm)

opDotColor :: VL -> DotColor
opDotColor _ = Gray
-- Dot colors
data DotColor = Tomato
              | Salmon
              | Gray
              | DimGray
              | Gold
              | Tan
              | Red
              | Crimson
              | Green
              | Sienna
              | Beige
              | DodgerBlue
              | LightSkyBlue

renderColor :: DotColor -> Doc
renderColor Tomato = text "tomato"
renderColor Salmon = text "salmon"
renderColor Gray = text "gray"
renderColor DimGray = text "dimgray"
renderColor Gold = text "gold"
renderColor Tan = text "tan"
renderColor Red = text "red"
renderColor Crimson = text "crimson"
renderColor Green = text "green"
renderColor Sienna = text "sienna"
renderColor Beige = text "beige"
renderColor DodgerBlue = text "dodgerblue"
renderColor LightSkyBlue = text "lightskyblue"

escapeLabel :: String -> String
escapeLabel s = concatMap escapeChar s

escapeChar :: Char -> [Char]
escapeChar '\n' = ['\\', 'n']
escapeChar '\\' = ['\\', '\\']
escapeChar '\"' = ['\\', '"']
escapeChar c = [c]

-- Type of Dot style options
data DotStyle = Solid

-- label of Dot nodes
type DotLabel = String

-- id of Dot nodes
type DotNodeID = Int

-- Type of Dot nodes
data DotNode = DotNode DotNodeID DotLabel DotColor (Maybe DotStyle)

-- Type of Dot edges
data DotEdge = DotEdge DotNodeID DotNodeID

-- Generate the preamble of a Dot file
preamble :: Doc
preamble = graphAttributes $$ nodeAttributes
    where nodeAttributes = text "node" <+> (brackets $ text "style=filled" <> comma <+> text "shape=box") <> semi
          graphAttributes = text "ordering=out;"

renderDotNode :: DotNode -> Doc
renderDotNode (DotNode n l c s) =
    int n
    <+> (brackets $ (((text "label=") <> (doubleQuotes $ text l))
                     <> comma
                     <+> (text "color=") <> (renderColor c)
                     <> styleDoc))
    <> semi
    where styleDoc =
              case s of
                  Just Solid -> comma <+> text "solid"
                  Nothing -> empty

renderDotEdge :: DotEdge -> Doc
renderDotEdge (DotEdge u v) = int u <+> text "->" <+> int v <> semi

-- | Render a Dot document from the preamble, nodes and edges
renderDot :: [DotNode] -> [DotEdge] -> Doc
renderDot ns es = text "digraph" <> (braces $ preamble $$ nodeSection $$ edgeSection)
    where nodeSection = vcat $ map renderDotNode ns
          edgeSection = vcat $ map renderDotEdge es

-- | Create an abstract Dot node from an X100 operator description
constructDotNode :: [AlgNode] -> NodeMap [Tag] -> (AlgNode, VL) -> DotNode
constructDotNode rootNodes ts (n, op) =
    if elem n rootNodes then
        DotNode n l c (Just Solid)
    else
        DotNode n l c Nothing
    where l = escapeLabel $ render $ opDotLabel ts n op
          c = opDotColor op

-- | Create an abstract Dot edge
constructDotEdge :: (AlgNode, AlgNode) -> DotEdge
constructDotEdge = uncurry DotEdge

-- | extract the operator descriptions and list of edges from a DAG
extractGraphStructure :: Dag.AlgebraDag VL
                     -> ([(AlgNode, VL)], [(AlgNode, AlgNode)])
extractGraphStructure d = (operators, childs)
    where nodes = Dag.topsort d
          operators = zip nodes $ map (flip Dag.operator d) nodes
          childs = concat $ map (\(n, op) -> zip (repeat n) (Dag.opChildren op)) operators

-- | Render an VL plan into a dot file (GraphViz).
renderVLDot :: NodeMap [Tag] -> [AlgNode] -> NodeMap VL -> String
renderVLDot ts roots m = render $ renderDot dotNodes dotEdges
    where (opLabels, edges) = extractGraphStructure dPruned
          d = Dag.mkDag m roots
          dPruned = case Dag.pruneUnused d of
                      Just d' -> d'
                      Nothing  -> d
          dotNodes = map (constructDotNode roots ts) opLabels
          dotEdges = map constructDotEdge edges