-- Dot

module Database.Algebra.Pathfinder.Render.Dot(renderPFDot) where

import qualified Data.IntMap                         		as Map
import           Data.List 							 		--as List

import           Text.PrettyPrint 					 		--as Pretty

import qualified Database.Algebra.Dag                		as Dag
import           Database.Algebra.Dag.Common		 		--as DagC
import           Database.Algebra.Pathfinder.Data.Algebra


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

-- | Rendering of the operations for opDotLabel
renderPosSel :: SemInfPosSel -> Doc
renderPosSel (i, sortInf, Just attr) = text $ show i ++ show sortInf ++ attr
renderPosSel (i, sortInf, Nothing) = text $ show i ++ show sortInf

renderAggr :: (AggrType, ResAttrName) -> Doc
renderAggr (aggr, res) = text $ res ++ ":" ++ show aggr

renderSortInf :: (SortAttrName, SortDir) -> Doc
renderSortInf (attr, Desc) = text $ attr ++ "/desc"
renderSortInf (attr, Asc)  = text attr

renderJoinArgs :: (LeftAttrName, RightAttrName, JoinRel) -> Doc
renderJoinArgs (left, right, joinR) = 
    text left <+> (text $ show joinR) <+> text right
    
renderOptCol :: Maybe AttrName -> Doc
renderOptCol Nothing  = empty
renderOptCol (Just c) = text "/" <> text c

renderKey :: Key -> Doc
renderKey (Key k) = brackets $ commas text k

renderColumn :: (AttrName, ATy) -> Doc
renderColumn (c, t) = text c <> text "::" <> (text $ show t)

renderTableInfo :: TableName -> [(AttrName, ATy)] -> [Key] -> Doc
renderTableInfo tableName cols keys = 
    (text tableName)
    <> text "\\n"
    <> (brackets $ commas renderColumn cols)
    <> text "\\n"
    <> (brackets $ commas renderKey keys)

opDotLabel :: NodeMap [Tag] -> AlgNode -> PFLabel -> Doc
-- | Nullary operations
opDotLabel tags i (LitTableL _ _)             = labelToDoc i 
    "LITTABLE" empty (lookupTags i tags)
opDotLabel tags i (EmptyTableL _)             = labelToDoc i 
    "EMPTYTABLE" empty (lookupTags i tags)
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
opDotLabel tags i (PosSelL info)              = labelToDoc i
    "POSSEL" (renderPosSel info) (lookupTags i tags)
opDotLabel tags i (DistinctL _)               = labelToDoc i
    "DISTINCT" empty (lookupTags i tags)
opDotLabel tags i (AggrL (aggrList, attr))    = labelToDoc i
    "AGGR" ((commas renderAggr aggrList) <+> (brackets $ commas renderProj attr))
    (lookupTags i tags)

constructDotNode :: NodeMap [Tag] -> (AlgNode, PFLabel) -> DotNode
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
renderColor Tomato       = text "tomato"
renderColor Red          = text "red"
renderColor OrangeRed    = text "orangered"
renderColor Salmon       = text "salmon"
renderColor Gray         = text "gray"
renderColor DimGray      = text "dimgray"
renderColor Gold         = text "gold"
renderColor Tan          = text "tan"
renderColor Crimson      = text "crimson"
renderColor Green        = text "green"
renderColor Sienna       = text "sienna"
renderColor Beige        = text "beige"
renderColor DodgerBlue   = text "dodgerblue"
renderColor LightSkyBlue = text "lightskyblue"
renderColor Gray52       = text "gray52"
renderColor Gray91       = text "gray91"
renderColor DarkOrange   = text "darkorange"
renderColor Orange       = text "orange"
renderColor White        = text "white"
renderColor Cyan         = text "cyan"
renderColor Cyan4        = text "cyan4"

opDotColor :: PFLabel -> DotColor

-- | Nullaryops
opDotColor (EmptyTableL _)   = Gray52
opDotColor (LitTableL _ _)   = Gray52
opDotColor (TableRefL _)     = Gray52

-- | Unops
opDotColor (ProjectL _)         = Gray91

opDotColor (PosSelL _)       = Cyan4
opDotColor (SelL _)          = Cyan

opDotColor (DistinctL _)     = Tan
opDotColor (AggrL _)         = Gold

opDotColor (RankL _)         = Tomato
opDotColor (RowNumL _)       = Red
opDotColor (RowRankL _)      = Red

-- | Binops
opDotColor (CrossL     _)    = OrangeRed

opDotColor (DifferenceL _)   = DarkOrange
opDotColor (DisjUnionL _)    = Orange

opDotColor (EqJoinL    _)    = Green

opDotColor (ThetaJoinL _)    = DodgerBlue
opDotColor (SemiJoinL _)     = LightSkyBlue
opDotColor (AntiJoinL _)     = LightSkyBlue

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
data DotColor = Tomato
              | Salmon
              | Gray
              | DimGray
              | Gold
              | Tan
              | Red
              | OrangeRed
              | Crimson
              | Green
              | Sienna
              | Beige
              | DodgerBlue
              | LightSkyBlue
              | Gray91
              | Gray52
              | DarkOrange
              | Orange
              | Cyan
              | Cyan4
              | White

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
data PFLabel = EmptyTableL SchemaInfos
             | LitTableL [Tuple] SchemaInfos
             | TableRefL SemInfTableRef    -- nullops
             | AggrL SemInfAggr
             | DistinctL ()
             | ProjectL [Proj]
             | RankL SemInfRank
             | RowNumL SemInfRowNum
             | RowRankL SemInfRank
             | SelL Expr
             | PosSelL SemInfPosSel        -- unops
             | CrossL ()
             | DifferenceL ()
             | DisjUnionL ()
             | EqJoinL SemInfEqJoin
             | ThetaJoinL SemInfJoin  -- binops
             | SemiJoinL SemInfJoin
             | AntiJoinL SemInfJoin

labelOfOp :: PFAlgebra -> PFLabel
labelOfOp (TerOp _ _ _ _)   = error "no tertiary operations"
labelOfOp (Database.Algebra.Dag.Common.BinOp op _ _)	= labelOfBinOp op
labelOfOp (Database.Algebra.Dag.Common.UnOp op _)		= labelOfUnOp op
labelOfOp (Database.Algebra.Dag.Common.NullaryOp op)	= labelOfNullaryOp op

labelOfBinOp :: BinOp -> PFLabel
labelOfBinOp (Cross info)     	= CrossL info
labelOfBinOp (Difference info)  = DifferenceL info
labelOfBinOp (DisjUnion info) 	= DisjUnionL info
labelOfBinOp (EqJoin info)	= EqJoinL info
labelOfBinOp (ThetaJoin info)   = ThetaJoinL info
labelOfBinOp (SemiJoin info)    = SemiJoinL info
labelOfBinOp (AntiJoin info)    = AntiJoinL info

labelOfUnOp :: UnOp -> PFLabel
labelOfUnOp (Aggr info)     = AggrL info
labelOfUnOp (Distinct info) = DistinctL  info
labelOfUnOp (Project info)  = ProjectL info
labelOfUnOp (Rank info)     = RankL info
labelOfUnOp (RowNum info)   = RowNumL info
labelOfUnOp (RowRank info)  = RowRankL info
labelOfUnOp (Select info)   = SelL info

labelOfNullaryOp :: NullOp -> PFLabel
labelOfNullaryOp (LitTable  info1 info2) = LitTableL info1 info2
labelOfNullaryOp (TableRef  info)      	 = TableRefL info

-- | extract the operator descriptions and list of edges from a DAG

extractGraphStructure :: Dag.Operator a => (a -> PFLabel)
                     -> Dag.AlgebraDag a
                     -> ([(AlgNode, PFLabel)], [(AlgNode, AlgNode)])
extractGraphStructure toLabel d = (labels, childs)
    where nodes = Dag.topsort d
          operators = zip nodes $ map (flip Dag.operator d) nodes
          labels = map (\(n, op) -> (n, toLabel op)) operators
          childs = concat $ map (\(n, op) -> zip (repeat n) (Dag.opChildren op)) operators

-- | Render an PFAlgebra plan into a dot file (GraphViz).
renderPFDot :: NodeMap [Tag] -> [AlgNode] -> NodeMap PFAlgebra -> String
renderPFDot ts roots m = render $ renderDot dotNodes dotEdges
    where (opLabels, edges) = extractGraphStructure labelOfOp d
          d = Dag.mkDag m roots
          dotNodes = map (constructDotNode ts) opLabels
          dotEdges = map constructDotEdge edges
