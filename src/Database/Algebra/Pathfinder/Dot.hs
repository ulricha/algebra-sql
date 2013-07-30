-- Dot

module Database.Algebra.Pathfinder.Dot(renderPFDot) where

import qualified Data.IntMap                         		as Map
import           Data.List 							 		--as List

import           Text.PrettyPrint 					 		--as Pretty

import qualified Database.Algebra.Dag                		as Dag
import           Database.Algebra.Dag.Common		 		--as DagC
import           Database.Algebra.Pathfinder.Data.Algebra   as Alg
--import           Database.Algebra.X100.Render.Common

nodeToDoc :: AlgNode -> Doc
nodeToDoc n = (text "id:") <+> (int n)

tagsToDoc :: [Tag] -> Doc
tagsToDoc ts = vcat $ map text ts

labelToDoc :: AlgNode -> String -> [Tag] -> Doc
labelToDoc n s ts = (nodeToDoc n) $$ (text s) <> (tagsToDoc $ nub ts)


lookupTags :: AlgNode -> NodeMap [Tag] -> [Tag]
lookupTags n m = Map.findWithDefault [] n m



renderBinOp :: BinOp -> Doc
renderBinOp (Cross ())        = text "Cross"
renderBinOp (EqJoin _)        = text "EqJoin"
renderBinOp (Difference ())   = text "Diff"
renderBinOp (DisjUnion ())    = text "Dis"
renderBinOp (ThetaJoin _)     = text "theta"

renderUnOp :: UnOp -> Doc
renderUnOp (RowNum _)     = text "RowNum"
renderUnOp (RowRank _)    = text "RowRank"
renderUnOp (Proj _)       = text "Proj"
renderUnOp (Sel _)        = text "Sel"
renderUnOp (PosSel _)     = text "possel"
renderUnOp (Distinct ())  = text "Distinct"
renderUnOp (Attach _)     = text "Attach"
renderUnOp (FunBinOp _)   = text "Funbin"
renderUnOp (Cast _)       = text "Cast"
renderUnOp (FunBoolNot _) = text "funboolnot"
renderUnOp (Aggr _)       = text "aggr"
renderUnOp (Dummy _)      = text "dummy"

renderNullOp :: NullOp -> Doc
renderNullOp (LitTable _ _) = text "LitTable"
renderNullOp (EmptyTable _) = text "EmptyTable"
renderNullOp (TableRef _)   = text "TableRef"

data Op = NullaryOp | UnaryOp | BinOp 
--renderOp :: Op -> Doc


bracketList :: (a -> Doc) -> [a] -> Doc
bracketList f = brackets . hsep . punctuate comma . map f




bracketListLines :: (a -> Doc) -> [a] -> Doc
bracketListLines f = brackets . vcat . punctuate comma . map f


escapeLabel :: String -> String
escapeLabel s = concatMap escapeChar s

escapeChar :: Char -> [Char]
escapeChar '\n' = ['\\', 'n']
escapeChar '\\' = ['\\', '\\']
escapeChar '\"' = ['\\', '"']
escapeChar c = [c]



constructDotNode :: [AlgNode] -> NodeMap [Tag] -> (AlgNode, PFLabel) -> DotNode
constructDotNode rootNodes ts (n, op) =
    DotNode n l c Nothing
      where l = "F" -- escapeLabel $ render $ opDotLabel ts n op
            c = Tomato

-- | Create an abstract Dot edge
constructDotEdge :: (AlgNode, AlgNode) -> DotEdge
constructDotEdge = uncurry DotEdge



renderDotEdge :: DotEdge -> Doc
renderDotEdge (DotEdge u v) = int u <+> text "->" <+> int v <> semi



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


-- | für renderDot

-- Generate the preamble of a Dot file
preamble :: Doc
preamble = graphAttributes $$ nodeAttributes
    where nodeAttributes = text "node" <+> (brackets $ text "style=filled" <> comma <+> text "shape=box") <> semi
          graphAttributes = text "ordering=out;"


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




-- | Render a Dot document from the preamble, nodes and edges
renderDot :: [DotNode] -> [DotEdge] -> Doc
renderDot ns es = text "digraph" <> (braces $ preamble $$ nodeSection $$ edgeSection)
    where nodeSection = vcat $ map renderDotNode ns
          edgeSection = vcat $ map renderDotEdge es



-- | für Label Type
data PFLabel = EmptyTableL
			 | LitTableL
			 | TableRefL	-- nullops
			 | AggrL
			 | AttachL
			 | CastL
			 | DistinctL
			 | DummyL
			 | FunBinOpL
			 | FunBoolNotL
			 | ProjL
			 | RankL
			 | RowNumL
			 | RowRankL
			 | PosSelL 			-- unops
			 | CrossL
			 | DifferenceL
			 | DisjUnionL
			 | EqJoinL
			 | ThetaJoinL 	-- binops

labelOfOp :: PFAlgebra -> PFLabel
labelOfOp (TerOp _ _ _ _)   = error "keine tertiäre Operation"
labelOfOp (Database.Algebra.Dag.Common.BinOp op _ _)	= labelOfBinOp op
labelOfOp (Database.Algebra.Dag.Common.UnOp op _)		= labelOfUnOp op
labelOfOp (Database.Algebra.Dag.Common.NullaryOp op)	= labelOfNullaryOp op

labelOfBinOp :: BinOp -> PFLabel
labelOfBinOp (Cross _)     		= CrossL
labelOfBinOp (Difference ())  = DifferenceL
labelOfBinOp (DisjUnion ()) 	= DisjUnionL
labelOfBinOp (EqJoin _)	    	= EqJoinL
labelOfBinOp (ThetaJoin _)   	= ThetaJoinL

labelOfUnOp :: UnOp -> PFLabel
labelOfUnOp (Aggr _) 		   	= AggrL
labelOfUnOp (Attach _) 	   	= AttachL
labelOfUnOp (Cast _) 		   	= CastL
labelOfUnOp (Distinct _) 		= DistinctL
labelOfUnOp (Dummy _) 	   	= DummyL
labelOfUnOp (FunBinOp _) 		= FunBinOpL
labelOfUnOp (FunBoolNot _) 	= FunBoolNotL
labelOfUnOp (PosSel _)      = PosSelL
labelOfUnOp (Proj _) 			  = ProjL
labelOfUnOp (Rank _) 		   	= RankL
labelOfUnOp (RowNum _) 	  	= RowNumL
labelOfUnOp (RowRank _) 		= RowRankL

labelOfNullaryOp :: NullOp -> PFLabel
labelOfNullaryOp (EmptyTable _)	= EmptyTableL
labelOfNullaryOp (LitTable _ _)	= LitTableL
labelOfNullaryOp (TableRef _) 	= TableRefL



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
          dotNodes = map (constructDotNode roots ts) opLabels
          dotEdges = map constructDotEdge edges




