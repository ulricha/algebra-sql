module Database.Algebra.Table.Render.Dot(renderTADot) where

import qualified Data.IntMap                 as Map
import           Data.List
import           Data.List.NonEmpty          (NonEmpty, toList)

import           Text.PrettyPrint

import qualified Database.Algebra.Dag        as Dag
import           Database.Algebra.Dag.Common
import           Database.Algebra.Table.Lang


nodeToDoc :: AlgNode -> Doc
nodeToDoc n = (text "id:") <+> (int n)

tagsToDoc :: [Tag] -> Doc
tagsToDoc ts = vcat $ map text ts

labelToDoc :: AlgNode -> String -> Doc -> [Tag] -> Doc
labelToDoc n s as ts = (nodeToDoc n) <> text "\\n" <> ((text s) <> (parens as)) <+> (tagsToDoc $ nub ts)

lookupTags :: AlgNode -> NodeMap [Tag] -> [Tag]
lookupTags n m = Map.findWithDefault [] n m

commas :: (a -> Doc) -> [a] -> Doc
commas f = hsep . punctuate comma . map f

renderProj :: Proj -> Doc
renderProj (new, ColE c) | new == c = text new
renderProj (new, e)                 = text $ concat [new, ":", show e]

renderAggr :: (AggrType, ResAttr) -> Doc
renderAggr (aggr, res) = text $ res ++ ":" ++ show aggr

renderSortInf :: SortSpec -> Doc
renderSortInf (attr, Desc) = text $ attr ++ "/desc"
renderSortInf (attr, Asc)  = text attr

renderJoinArgs :: (Expr, Expr, JoinRel) -> Doc
renderJoinArgs (left, right, joinR) =
    (text $ show left) <+> (text $ show joinR) <+> (text $ show right)

renderOptCol :: Maybe Attr -> Doc
renderOptCol Nothing  = empty
renderOptCol (Just c) = text "/" <> text c

renderKey :: Key -> Doc
renderKey (Key k) = brackets $ commas text k

renderColumn :: (Attr, ATy) -> Doc
renderColumn (c, t) = text c <> text "::" <> (text $ show t)

renderTuple :: Tuple -> Doc
renderTuple = hcat . punctuate comma . map (text . show)

renderData :: [Tuple] -> Doc
renderData [] = empty
renderData xs = sep $ punctuate semi $ map renderTuple xs

renderTableInfo :: TableName -> [(Attr, ATy)] -> [Key] -> Doc
renderTableInfo tableName cols keys =
    (text tableName)
    <> text "\\n"
    <> (brackets $ commas renderColumn cols)
    <> text "\\n"
    <> (brackets $ commas renderKey keys)

opDotLabel :: NodeMap [Tag] -> AlgNode -> TALabel -> Doc
-- | Nullary operations
opDotLabel tags i (LitTableL dat _schema)      = labelToDoc i
    "LITTABLE" (renderData dat) (lookupTags i tags)
opDotLabel tags i (TableRefL (name, attrs, keys)) = labelToDoc i
    "TABLE" (renderTableInfo name attrs keys) (lookupTags i tags)
-- |  Binary operations
opDotLabel tags i (CrossL _)                  = labelToDoc i
    "CROSS" empty (lookupTags i tags)
opDotLabel tags i (EqJoinL (left,right))      = labelToDoc i
    "EQJOIN" (text $ left ++ "," ++ right) (lookupTags i tags)
opDotLabel tags i (DifferenceL _)             = labelToDoc i
    "DIFF" empty (lookupTags i tags)
opDotLabel tags i (DisjUnionL _)              = labelToDoc i
    "UNION" empty (lookupTags i tags)
opDotLabel tags i (ThetaJoinL info)           = labelToDoc i
    "THETAJOIN" (commas renderJoinArgs info) (lookupTags i tags)
opDotLabel tags i (SemiJoinL info)           = labelToDoc i
    "SEMIJOIN" (commas renderJoinArgs info) (lookupTags i tags)
opDotLabel tags i (AntiJoinL info)           = labelToDoc i
    "ANTIJOIN" (commas renderJoinArgs info) (lookupTags i tags)
-- | Unary operations
opDotLabel tags i (RowNumL (res,sortI,attr))  = labelToDoc i
    "ROWNUM" ((text $ res ++ ":<")
              <> (commas renderSortInf sortI)
              <> text ">"
              <> renderOptCol attr)
    (lookupTags i tags)
opDotLabel tags i (RowRankL (res,sortInf))    = labelToDoc i
    "ROWRANK" ((text $ res ++ ":<")
               <> (commas renderSortInf sortInf)
               <> text ">")
    (lookupTags i tags)
opDotLabel tags i (RankL (res,sortInf))       = labelToDoc i
    "RANK" ((text $ res ++ ":<")
            <> commas renderSortInf sortInf
            <> text ">")
    (lookupTags i tags)
opDotLabel tags i (ProjectL info)                = labelToDoc i
    "PROJECT" (commas renderProj info) (lookupTags i tags)
opDotLabel tags i (SelL info)                 = labelToDoc i
    "SELECT" (text $ show info) (lookupTags i tags)
opDotLabel tags i (DistinctL _)               = labelToDoc i
    "DISTINCT" empty (lookupTags i tags)
opDotLabel tags i (AggrL (aggrList, attr))    = labelToDoc i
    "AGGR" ((commas renderAggr aggrList) <+> (brackets $ commas renderProj attr))
    (lookupTags i tags)
opDotLabel tags i (SerializeL (mDescr, mPos, cols)) = labelToDoc i
    "SERIALIZE" (renderSerCol mDescr
                 <+> (text $ show mPos)
                 <+> (brackets $ commas (text . show) cols))
    (lookupTags i tags)
opDotLabel tags i (WinFunL (winFuns, partSpec, sortSpec, mFrameBounds)) = labelToDoc i
     "WIN" (vcat [ renderWinFuns winFuns
                 , renderPartSpec partSpec
                 , renderSortSpec sortSpec
                 , maybe empty renderFrameBounds mFrameBounds
                 ])
     (lookupTags i tags)

renderWinFun :: WinFun -> Doc
renderWinFun (WinMax e) = text "MAX" <> (parens $ text $ show e)
renderWinFun (WinMin e) = text "MAX" <> (parens $ text $ show e)
renderWinFun (WinSum e) = text "MAX" <> (parens $ text $ show e)

renderWinFuns :: NonEmpty (ResAttr, WinFun) -> Doc
renderWinFuns ws = commas (\(c, f) -> renderWinFun f <+> text "AS" <+> text c) $ toList ws

renderPartSpec :: [PartAttr] -> Doc
renderPartSpec []       = empty
renderPartSpec as@(_:_) = text "PARTITION BY" <+> commas text as

renderSortSpec :: [SortSpec] -> Doc
renderSortSpec [] = empty
renderSortSpec ss@(_:_) = text "ORDER BY" <+> commas renderSortInf ss

renderFrameBounds :: FrameBounds -> Doc
renderFrameBounds (HalfOpenFrame fs)  = renderFrameStart fs
renderFrameBounds (ClosedFrame fs fe) = renderFrameStart fs 
                                        <+> text "AND" 
                                        <+> renderFrameEnd fe

renderFrameStart :: FrameStart -> Doc
renderFrameStart FSUnboundPrec = text "UNBOUNDED PRECEDING"
renderFrameStart (FSValPrec i) = int i <+> text "PRECEDING"
renderFrameStart FSCurrRow     = text "CURRENT ROW"

renderFrameEnd :: FrameEnd -> Doc
renderFrameEnd FEUnboundFol = text "UNBOUNDED FOLLOWING"
renderFrameEnd (FEValFol i) = int i <+> text "FOLLOWING"
renderFrameEnd FECurrRow    = text "CURRENT ROW"

renderSerCol :: Show c => Maybe c -> Doc
renderSerCol Nothing  = empty
renderSerCol (Just c) = (text $ show c) <> comma

constructDotNode :: NodeMap [Tag] -> (AlgNode, TALabel) -> DotNode
constructDotNode tags (n, op) =
    DotNode n l c Nothing
      where l = render $ opDotLabel tags n op
            c = opDotColor op

-- | Create an abstract Dot edge
constructDotEdge :: (AlgNode, AlgNode) -> DotEdge
constructDotEdge = uncurry DotEdge

renderDotEdge :: DotEdge -> Doc
renderDotEdge (DotEdge u v) = int u <+> text "->" <+> int v <> semi

renderColor :: DotColor -> Doc
renderColor DCTomato       = text "tomato"
renderColor DCRed          = text "red"
renderColor DCOrangeDCRed    = text "orangered"
renderColor DCSalmon       = text "salmon"
renderColor DCGray         = text "gray"
renderColor DCDimDCGray      = text "dimgray"
renderColor DCGold         = text "gold"
renderColor DCTan          = text "tan"
renderColor DCCrimson      = text "crimson"
renderColor DCGreen        = text "green"
renderColor DCSienna       = text "sienna"
renderColor DCBeige        = text "beige"
renderColor DCDodgerBlue   = text "dodgerblue"
renderColor DCLightSkyBlue = text "lightskyblue"
renderColor DCGray52       = text "gray52"
renderColor DCGray91       = text "gray91"
renderColor DCDarkDCOrange   = text "darkorange"
renderColor DCOrange       = text "orange"
renderColor DCWhite        = text "white"
renderColor DCCyan         = text "cyan"
renderColor DCCyan4        = text "cyan4"
renderColor DCHotPink      = text "hotpink"

opDotColor :: TALabel -> DotColor

-- | Nullaryops
opDotColor (LitTableL _ _)   = DCGray52
opDotColor (TableRefL _)     = DCGray52

-- | Unops
opDotColor (ProjectL _)      = DCGray91
opDotColor (SerializeL _)    = DCHotPink

opDotColor (SelL _)          = DCCyan

opDotColor (DistinctL _)     = DCTan
opDotColor (AggrL _)         = DCGold

opDotColor (RankL _)         = DCTomato
opDotColor (RowNumL _)       = DCRed
opDotColor (RowRankL _)      = DCRed
opDotColor (WinFunL _)       = DCSalmon

-- | Binops
opDotColor (CrossL     _)    = DCOrangeDCRed

opDotColor (DifferenceL _)   = DCDarkDCOrange
opDotColor (DisjUnionL _)    = DCOrange

opDotColor (EqJoinL    _)    = DCGreen

opDotColor (ThetaJoinL _)    = DCDodgerBlue
opDotColor (SemiJoinL _)     = DCLightSkyBlue
opDotColor (AntiJoinL _)     = DCLightSkyBlue

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

preamble :: Doc
preamble = graphAttributes $$ nodeAttributes
    where nodeAttributes = text "node" <+> (brackets $ text "style=filled" <> comma <+> text "shape=box") <> semi
          graphAttributes = text "ordering=out;"

-- | Dot colors
data DotColor = DCTomato
              | DCSalmon
              | DCGray
              | DCDimDCGray
              | DCGold
              | DCTan
              | DCRed
              | DCOrangeDCRed
              | DCCrimson
              | DCGreen
              | DCSienna
              | DCBeige
              | DCDodgerBlue
              | DCLightSkyBlue
              | DCGray91
              | DCGray52
              | DCDarkDCOrange
              | DCOrange
              | DCCyan
              | DCCyan4
              | DCWhite
              | DCHotPink

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

-- |  Render a Dot document from the preamble, nodes and edges
renderDot :: [DotNode] -> [DotEdge] -> Doc
renderDot ns es = text "digraph" <> (braces $ preamble $$ nodeSection $$ edgeSection)
    where nodeSection = vcat $ map renderDotNode ns
          edgeSection = vcat $ map renderDotEdge es

-- | Labels (to collect all operations (nullary, unary,binary))
data TALabel = LitTableL [Tuple] SchemaInfos
             | TableRefL (TableName, [TypedAttr], [Key])
             | AggrL ([(AggrType, ResAttr)], [(PartAttr, Expr)])
             | WinFunL (NonEmpty (ResAttr, WinFun), [PartAttr], [SortSpec], Maybe FrameBounds)
             | DistinctL ()
             | ProjectL [Proj]
             | RankL (ResAttr, [SortSpec])
             | RowNumL (Attr, [SortSpec], Maybe PartAttr)
             | RowRankL (ResAttr, [SortSpec])
             | SelL Expr
             | CrossL ()
             | DifferenceL ()
             | DisjUnionL ()
             | EqJoinL (LeftAttr,RightAttr)
             | ThetaJoinL [(Expr, Expr, JoinRel)]
             | SemiJoinL [(Expr, Expr, JoinRel)]
             | AntiJoinL [(Expr, Expr, JoinRel)]
             | SerializeL (Maybe DescrCol, SerializeOrder, [PayloadCol])

labelOfOp :: TableAlgebra -> TALabel
labelOfOp (Database.Algebra.Dag.Common.BinOp op _ _) = labelOfBinOp op
labelOfOp (Database.Algebra.Dag.Common.UnOp op _)    = labelOfUnOp op
labelOfOp (Database.Algebra.Dag.Common.NullaryOp op) = labelOfNullaryOp op
labelOfOp (TerOp _ _ _ _)                            = error "no tertiary operations"

labelOfBinOp :: BinOp -> TALabel
labelOfBinOp (Cross info)     	= CrossL info
labelOfBinOp (Difference info)  = DifferenceL info
labelOfBinOp (DisjUnion info) 	= DisjUnionL info
labelOfBinOp (EqJoin info)	= EqJoinL info
labelOfBinOp (ThetaJoin info)   = ThetaJoinL info
labelOfBinOp (SemiJoin info)    = SemiJoinL info
labelOfBinOp (AntiJoin info)    = AntiJoinL info

labelOfUnOp :: UnOp -> TALabel
labelOfUnOp (WinFun info)    = WinFunL info
labelOfUnOp (Aggr info)      = AggrL info
labelOfUnOp (Distinct info)  = DistinctL  info
labelOfUnOp (Project info)   = ProjectL info
labelOfUnOp (Rank info)      = RankL info
labelOfUnOp (RowNum info)    = RowNumL info
labelOfUnOp (RowRank info)   = RowRankL info
labelOfUnOp (Select info)    = SelL info
labelOfUnOp (Serialize info) = SerializeL info

labelOfNullaryOp :: NullOp -> TALabel
labelOfNullaryOp (LitTable  info1 info2) = LitTableL info1 info2
labelOfNullaryOp (TableRef  info)      	 = TableRefL info

-- | extract the operator descriptions and list of edges from a DAG

extractGraphStructure :: Dag.Operator a => (a -> TALabel)
                     -> Dag.AlgebraDag a
                     -> ([(AlgNode, TALabel)], [(AlgNode, AlgNode)])
extractGraphStructure toLabel d = (labels, childs)
    where nodes = Dag.topsort d
          operators = zip nodes $ map (flip Dag.operator d) nodes
          labels = map (\(n, op) -> (n, toLabel op)) operators
          childs = concat $ map (\(n, op) -> zip (repeat n) (Dag.opChildren op)) operators

-- | Render an TableAlgebra plan into a dot file (GraphViz).
renderTADot :: NodeMap [Tag] -> [AlgNode] -> NodeMap TableAlgebra -> String
renderTADot ts roots m = render $ renderDot dotNodes dotEdges
    where (opLabels, edges) = extractGraphStructure labelOfOp d
          d = Dag.mkDag m roots
          dotNodes = map (constructDotNode ts) opLabels
          dotEdges = map constructDotEdge edges
