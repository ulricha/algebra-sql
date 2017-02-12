module Database.Algebra.Table.Render.Dot(renderTADot) where


import           Data.List

import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Database.Algebra.Dag         as Dag
import           Database.Algebra.Dag.Common
import           Database.Algebra.Table.Lang

pp :: P.Pretty a => a -> String
pp a = (P.displayS $ P.renderPretty 0.9 120 $ P.pretty a) ""

nodeToDoc :: AlgNode -> P.Doc
nodeToDoc n = (P.text "id:") P.<+> (P.int n)

labelToDoc :: AlgNode -> String -> P.Doc -> P.Doc
labelToDoc n s as = (nodeToDoc n) P.<+> P.text "\\n" P.<+> ((P.text s) P.<> (P.parens as))

commas :: (a -> P.Doc) -> [a] -> P.Doc
commas f = P.hsep . P.punctuate P.comma . map f

renderProj :: Proj -> P.Doc
renderProj (new, ColE c) | new == c = P.text new
renderProj (new, e)                 = P.text new P.<> P.colon P.<> P.pretty e

renderAggr :: (AggrType, ResAttr) -> P.Doc
renderAggr (aggr, res) = P.text res P.<> P.colon P.<> P.pretty aggr

renderSortInf :: SortSpec -> P.Doc
renderSortInf (ColE c, Desc) = P.text c P.<+> P.text "/desc"
renderSortInf (expr, Desc)   = (P.parens $ P.pretty expr) P.<> P.text "/desc"
renderSortInf (ColE c, Asc)  = P.text c
renderSortInf (expr, Asc)    = P.parens $ P.pretty expr


renderJoinArgs :: (Expr, Expr, JoinRel) -> P.Doc
renderJoinArgs (left, right, joinR) =
    parenthize left P.<+> P.pretty joinR P.<+> parenthize right

renderPartExprs :: [PartExpr] -> P.Doc
renderPartExprs []       = P.empty
renderPartExprs es@(_:_) = P.text "/" P.<+> commas P.pretty es

renderKey :: Key -> P.Doc
renderKey (Key k) = P.brackets $ commas P.text k

renderColumn :: (Attr, ATy) -> P.Doc
renderColumn (c, t) = P.text c P.<+> P.text "::" P.<+> (P.pretty t) 

renderTuple :: Tuple -> P.Doc
renderTuple = P.hcat . P.punctuate P.comma . map P.pretty

renderData :: [Tuple] -> P.Doc
renderData [] = P.empty
renderData xs = P.sep $ P.punctuate P.semi $ map renderTuple xs

renderSchema :: [TypedAttr] -> P.Doc
renderSchema cols = commas renderColumn cols

renderTableInfo :: TableName -> [(Attr, ATy)] -> [Key] -> P.Doc
renderTableInfo tableName cols keys =
    (P.text tableName)
    P.<+> P.text "\\n"
    P.<+> (P.brackets $ renderSchema cols)
    P.<+> P.text "\\n"
    P.<+> (P.brackets $ commas renderKey keys)

opDotLabel :: AlgNode -> TALabel -> P.Doc
-- | Nullary operations
opDotLabel i (LitTableL dat schema)      = labelToDoc i
    "LITTABLE" (renderSchema schema P.<+> renderData dat)
opDotLabel i (TableRefL (name, attrs, keys)) = labelToDoc i
    "TABLE" (renderTableInfo name attrs keys)
-- |  Binary operations
opDotLabel i (CrossL _)                  = labelToDoc i
    "CROSS" P.empty
opDotLabel i (DifferenceL _)             = labelToDoc i
    "DIFF" P.empty
opDotLabel i (DisjUnionL _)              = labelToDoc i
    "UNION" P.empty
opDotLabel i (ThetaJoinL info)           = labelToDoc i
    "THETAJOIN" (commas renderJoinArgs info)
opDotLabel i (LeftOuterJoinL info)       = labelToDoc i
    "LEFTOUTERJOIN" (commas renderJoinArgs info)
opDotLabel i (SemiJoinL info)           = labelToDoc i
    "SEMIJOIN" (commas renderJoinArgs info)
opDotLabel i (AntiJoinL info)           = labelToDoc i
    "ANTIJOIN" (commas renderJoinArgs info)
-- | Unary operations
opDotLabel i (RowNumL (res,sortI,attr))  = labelToDoc i
    "ROWNUM" ((P.text $ res ++ ":<")
              P.<+> (commas renderSortInf sortI)
              P.<+> P.text ">"
              P.<+> renderPartExprs attr)
opDotLabel i (RowRankL (res,sortInf))    = labelToDoc i
    "ROWRANK" ((P.text $ res ++ ":<")
               P.<+> (commas renderSortInf sortInf)
               P.<+> P.text ">")
opDotLabel i (RankL (res,sortInf))       = labelToDoc i
    "RANK" ((P.text $ res ++ ":<")
            P.<+> commas renderSortInf sortInf
            P.<+> P.text ">")
opDotLabel i (ProjectL info)                = labelToDoc i
    "PROJECT" (commas renderProj info)
opDotLabel i (SelL info)                 = labelToDoc i
    "SELECT" (P.pretty info)
opDotLabel i (DistinctL _)               = labelToDoc i
    "DISTINCT" P.empty
opDotLabel i (AggrL (aggrList, attr))    = labelToDoc i
    "AGGR" (commas renderAggr aggrList
            P.<+>
            (P.brackets $ commas renderProj attr))
opDotLabel i (SerializeL (ref, key, ord, item)) = labelToDoc i
    "SERIALIZE" (serializeArg "ref" ref P.<+> P.text "\n"
                 P.<+> serializeArg "key" key P.<+> P.text "\n"
                 P.<+> serializeArg "ord" ord P.<+> P.text "\n"
                 P.<+> serializeArg "items" item P.<+> P.text "\n")
opDotLabel i (WinFunL (winFuns, partSpec, sortSpec, mFrameBounds)) = labelToDoc i
     "WIN" (P.hcat $ intersperse (P.text "\\n") [ renderWinFuns winFuns
                                                , renderPartSpec partSpec
                                                , renderSortSpec sortSpec
                                                , maybe P.empty
                                                        renderFrameBounds
                                                        mFrameBounds
                                                ])

serializeArg :: P.Pretty a => String -> [a] -> P.Doc
serializeArg desc cols = P.text desc P.<+> P.equals
                                     P.<+> P.brackets (commas P.pretty cols)

renderWinFun :: WinFun -> P.Doc
renderWinFun (WinMax e)        = P.text "MAX" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinMin e)        = P.text "MIN" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinSum e)        = P.text "SUM" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinAvg e)        = P.text "AVG" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinAll e)        = P.text "ALL" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinAny e)        = P.text "ANY" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinFirstValue e) = P.text "first_value" P.<+> (P.parens $ P.pretty e)
renderWinFun (WinLastValue e)  = P.text "last_value" P.<+> (P.parens $ P.pretty e)
renderWinFun WinCount          = P.text "COUNT()"

renderWinFuns :: (ResAttr, WinFun) -> P.Doc
renderWinFuns (c, f) = renderWinFun f P.<+> P.text "AS" P.<+> P.text c

renderPartSpec :: [PartExpr] -> P.Doc
renderPartSpec []       = P.empty
renderPartSpec as@(_:_) = P.text "PARTITION BY" P.<+> commas P.pretty as

renderSortSpec :: [SortSpec] -> P.Doc
renderSortSpec []       = P.empty
renderSortSpec ss@(_:_) = P.text "ORDER BY" P.<+> commas renderSortInf ss

renderFrameBounds :: FrameBounds -> P.Doc
renderFrameBounds (HalfOpenFrame fs)  = renderFrameStart fs
renderFrameBounds (ClosedFrame fs fe) = renderFrameStart fs
                                        P.<+> P.text "AND"
                                        P.<+> renderFrameEnd fe

renderFrameStart :: FrameStart -> P.Doc
renderFrameStart FSUnboundPrec = P.text "UNBOUNDED PRECEDING"
renderFrameStart (FSValPrec i) = P.int i P.<+> P.text "PRECEDING"
renderFrameStart FSCurrRow     = P.text "CURRENT ROW"

renderFrameEnd :: FrameEnd -> P.Doc
renderFrameEnd FEUnboundFol = P.text "UNBOUNDED FOLLOWING"
renderFrameEnd (FEValFol i) = P.int i P.<+> P.text "FOLLOWING"
renderFrameEnd FECurrRow    = P.text "CURRENT ROW"

constructDotNode :: (AlgNode, TALabel) -> DotNode
constructDotNode (n, op) =
    DotNode n l c Nothing
      where l = pp $ opDotLabel n op
            c = opDotColor op

-- | Create an abstract Dot edge
constructDotEdge :: (AlgNode, AlgNode) -> DotEdge
constructDotEdge = uncurry DotEdge

renderDotEdge :: DotEdge -> P.Doc
renderDotEdge (DotEdge u v) = P.int u P.<+> P.text "->" P.<+> P.int v P.<+> P.semi

renderColor :: DotColor -> P.Doc
renderColor DCTomato       = P.text "tomato"
renderColor DCRed          = P.text "red"
renderColor DCOrangeDCRed  = P.text "orangered"
renderColor DCSalmon       = P.text "salmon"
renderColor DCGray         = P.text "gray"
renderColor DCDimDCGray    = P.text "dimgray"
renderColor DCGold         = P.text "gold"
renderColor DCTan          = P.text "tan"
renderColor DCCrimson      = P.text "crimson"
renderColor DCGreen        = P.text "green"
renderColor DCSienna       = P.text "sienna"
renderColor DCBeige        = P.text "beige"
renderColor DCDodgerBlue   = P.text "dodgerblue"
renderColor DCLightSkyBlue = P.text "lightskyblue"
renderColor DCDeepSkyBlue  = P.text "deepskyblue"
renderColor DCGray52       = P.text "gray52"
renderColor DCGray91       = P.text "gray91"
renderColor DCDarkDCOrange = P.text "darkorange"
renderColor DCOrange       = P.text "orange"
renderColor DCWhite        = P.text "white"
renderColor DCCyan         = P.text "cyan"
renderColor DCCyan4        = P.text "cyan4"
renderColor DCHotPink      = P.text "hotpink"

opDotColor :: TALabel -> DotColor

-- | Nullaryops
opDotColor (LitTableL _ _)    = DCGray52
opDotColor (TableRefL _)      = DCGray52

-- | Unops
opDotColor (ProjectL _)       = DCGray91
opDotColor (SerializeL _)     = DCHotPink

opDotColor (SelL _)           = DCCyan

opDotColor (DistinctL _)      = DCTan
opDotColor (AggrL _)          = DCGold

opDotColor (RankL _)          = DCTomato
opDotColor (RowNumL _)        = DCRed
opDotColor (RowRankL _)       = DCRed
opDotColor (WinFunL _)        = DCSalmon

-- | Binops
opDotColor (CrossL     _)     = DCOrangeDCRed

opDotColor (DifferenceL _)    = DCDarkDCOrange
opDotColor (DisjUnionL _)     = DCOrange

opDotColor (ThetaJoinL _)     = DCDodgerBlue
opDotColor (LeftOuterJoinL _) = DCDeepSkyBlue
opDotColor (SemiJoinL _)      = DCLightSkyBlue
opDotColor (AntiJoinL _)      = DCLightSkyBlue

renderDotNode :: DotNode -> P.Doc
renderDotNode (DotNode n l c s) =
    P.int n
    P.<+> (P.brackets $ (((P.text "label=") P.<+> (P.dquotes $ P.text l))
                     P.<+> P.comma
                     P.<+> (P.text "color=") P.<+> (renderColor c)
                     P.<+> styleDoc))
    P.<+> P.semi
    where styleDoc =
              case s of
                  Just Solid -> P.comma P.<+> P.text "solid"
                  Nothing -> P.empty

preamble :: P.Doc
preamble = graphAttributes P.</> nodeAttributes
  where
    nodeAttributes = P.text "node [style=filled, shape=box];"
    graphAttributes = P.text "ordering=out;"

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
              | DCDeepSkyBlue
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
renderDot :: [DotNode] -> [DotEdge] -> P.Doc
renderDot ns es = P.text "digraph" P.<+> (P.braces $ preamble P.</> nodeSection P.</> edgeSection)
    where nodeSection = P.vcat $ map renderDotNode ns
          edgeSection = P.vcat $ map renderDotEdge es

-- | Labels (to collect all operations (nullary, unary,binary))
data TALabel = LitTableL [Tuple] [TypedAttr]
             | TableRefL (TableName, [TypedAttr], [Key])
             | AggrL ([(AggrType, ResAttr)], [(PartAttr, Expr)])
             | WinFunL ((ResAttr, WinFun), [PartExpr], [SortSpec], Maybe FrameBounds)
             | DistinctL ()
             | ProjectL [Proj]
             | RankL (ResAttr, [SortSpec])
             | RowNumL (Attr, [SortSpec], [PartExpr])
             | RowRankL (ResAttr, [SortSpec])
             | SelL Expr
             | CrossL ()
             | DifferenceL ()
             | DisjUnionL ()
             | ThetaJoinL [(Expr, Expr, JoinRel)]
             | SemiJoinL [(Expr, Expr, JoinRel)]
             | AntiJoinL [(Expr, Expr, JoinRel)]
             | LeftOuterJoinL [(Expr, Expr, JoinRel)]
             | SerializeL ([RefCol], [KeyCol], [OrdCol], [PayloadCol])

labelOfOp :: TableAlgebra -> TALabel
labelOfOp (Database.Algebra.Dag.Common.BinOp op _ _) = labelOfBinOp op
labelOfOp (Database.Algebra.Dag.Common.UnOp op _)    = labelOfUnOp op
labelOfOp (Database.Algebra.Dag.Common.NullaryOp op) = labelOfNullaryOp op
labelOfOp (TerOp _ _ _ _)                            = error "no tertiary operations"

labelOfBinOp :: BinOp -> TALabel
labelOfBinOp (Cross info)           = CrossL info
labelOfBinOp (Difference info)      = DifferenceL info
labelOfBinOp (DisjUnion info)       = DisjUnionL info
labelOfBinOp (ThetaJoin info)       = ThetaJoinL info
labelOfBinOp (SemiJoin info)        = SemiJoinL info
labelOfBinOp (AntiJoin info)        = AntiJoinL info
labelOfBinOp (LeftOuterJoin info)   = LeftOuterJoinL info

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
labelOfNullaryOp (LitTable  (tups, schema)) = LitTableL tups schema
labelOfNullaryOp (TableRef  info)           = TableRefL info

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
renderTADot :: [AlgNode] -> NodeMap TableAlgebra -> String
renderTADot roots m = pp $ renderDot dotNodes dotEdges
    where (opLabels, edges) = extractGraphStructure labelOfOp d
          d = Dag.mkDag m roots
          dotNodes = map constructDotNode opLabels
          dotEdges = map constructDotEdge edges
