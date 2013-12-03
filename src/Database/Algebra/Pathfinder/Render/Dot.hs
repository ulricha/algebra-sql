-- Dot

module Database.Algebra.Pathfinder.Render.Dot(renderPFDot) where

import qualified Data.IntMap                         		as Map
import           Data.List 							 		--as List

import           Text.PrettyPrint 					 		--as Pretty

import qualified Database.Algebra.Dag                		as Dag
import           Database.Algebra.Dag.Common		 		--as DagC
import           Database.Algebra.Pathfinder.Data.Algebra   as Alg


nodeToDoc :: AlgNode -> Doc
nodeToDoc n = (text "id:") <+> (int n)

tagsToDoc :: [Tag] -> Doc
tagsToDoc ts = vcat $ map text ts

labelToDoc :: AlgNode -> String -> Doc -> [Tag] -> Doc
labelToDoc n s as ts = (nodeToDoc n) $$ ((text s) <> (parens as)) $$ (tagsToDoc $ nub ts)


lookupTags :: AlgNode -> NodeMap [Tag] -> [Tag]
lookupTags n m = Map.findWithDefault [] n m

bracketList :: (a -> Doc) -> [a] -> Doc
bracketList f = brackets . hsep . punctuate comma . map f

renderProj :: ProjPair -> Doc
renderProj (new,old) = text $ concat [old,"->",new]

-- | Rendering of the operations for opDotLabel
renderPosSel :: SemInfPosSel -> Doc
renderPosSel (i, sortInf, Just attr) = text $ show i ++ show sortInf ++ attr
renderPosSel (i, sortInf, Nothing) = text $ show i ++ show sortInf

renderFunBinOp :: SemBinOp -> Doc
renderFunBinOp (fun, resAttr, leftAttr, rightAttr) = 
    text $ resAttr ++ "  " ++ show fun 
                   ++ ":" ++ leftAttr ++ "," ++ rightAttr

renderAggr :: (AggrType, ResAttrName) -> Doc
renderAggr (aggr, res) = text $ show aggr ++ ":" ++ res

renderSortInf :: (SortAttrName, SortDir) -> Doc
renderSortInf (attr, dir) = text $ attr ++ "/" ++ show dir

renderJoinArgs :: (LeftAttrName, RightAttrName, JoinRel) -> Doc
renderJoinArgs (left, right, joinR) = 
    text $ show joinR ++ ":" ++ left ++ "," ++ right

opDotLabel :: NodeMap [Tag] -> AlgNode -> PFLabel -> Doc
-- | Nullary operations
opDotLabel tags i (LitTableL _ _)             = labelToDoc i 
    "LitTable" empty (lookupTags i tags)
opDotLabel tags i (EmptyTableL _)             = labelToDoc i 
    "EmptyTable" empty (lookupTags i tags)
opDotLabel tags i (TableRefL _)               = labelToDoc i
    "TableRef" empty (lookupTags i tags)
-- |  Binary operations
opDotLabel tags i (CrossL _)                  = labelToDoc i
    "Cross" empty (lookupTags i tags)
opDotLabel tags i (EqJoinL (left,right))      = labelToDoc i
    "EqJoin" (text $ left ++ "," ++ right) (lookupTags i tags)
opDotLabel tags i (DifferenceL _)             = labelToDoc i
    "Difference" empty (lookupTags i tags)
opDotLabel tags i (DisjUnionL _)              = labelToDoc i
    "DisjUnion" empty (lookupTags i tags)
opDotLabel tags i (ThetaJoinL info)           = labelToDoc i
    "ThetaJoin" (bracketList renderJoinArgs info) (lookupTags i tags)
opDotLabel tags i (SemiJoinL info)           = labelToDoc i
    "SemiJoin" (bracketList renderJoinArgs info) (lookupTags i tags)
opDotLabel tags i (AntiJoinL info)           = labelToDoc i
    "AntiJoin" (bracketList renderJoinArgs info) (lookupTags i tags)
-- | Unary operations
opDotLabel tags i (RowNumL (res,sortI,attr))  = labelToDoc i 
    "RowNum" ((text $ res ++ "<")
              $$ bracketList renderSortInf sortI 
              $$ (text $ show attr ++ ">"))
    (lookupTags i tags)
opDotLabel tags i (RowRankL (res,sortInf))    = labelToDoc i
    "RowRank" ((text $ res ++ "<") 
               $$ bracketList renderSortInf sortInf
               $$ text "<")                
    (lookupTags i tags)
opDotLabel tags i (RankL (res,sortInf))       = labelToDoc i
    "Rank" ((text $ res ++ "<") 
            $$ bracketList renderSortInf sortInf
            $$ text ">")                
    (lookupTags i tags)
opDotLabel tags i (ProjL info)                = labelToDoc i 
    "Project" (bracketList renderProj info) (lookupTags i tags)
opDotLabel tags i (SelL info)                 = labelToDoc i
    "Select" (text info) (lookupTags i tags)
opDotLabel tags i (PosSelL info)              = labelToDoc i
    "PosSel" (renderPosSel info) (lookupTags i tags)
opDotLabel tags i (DistinctL _)               = labelToDoc i
    "Distinct" empty (lookupTags i tags)
opDotLabel tags i (AttachL (res, (aty,aval))) = labelToDoc i
    "Attach" (text $ res ++ "," ++ show aty ++ ":" ++ show aval)
    (lookupTags i tags)
opDotLabel tags i (FunBinOpL info)            = labelToDoc i
    "FunBinOp" (renderFunBinOp info) (lookupTags i tags) 
opDotLabel tags i (CastL (res,attr,aty))      = labelToDoc i
    "Cast" (text $ res ++ "," ++ show aty ++ ":" ++ attr) (lookupTags i tags)
opDotLabel tags i (FunBoolNotL (res,attr))    = labelToDoc i
    "FunBoolNot" (text $ res ++ "," ++ attr) (lookupTags i tags)
opDotLabel tags i (AggrL (aggrList, attr))    = labelToDoc i
    "Aggr" ((bracketList renderAggr aggrList) $$ (text $ show attr))
    (lookupTags i tags)
opDotLabel _ _ (DummyL _)                     = text "dummy"



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

opDotColor :: PFLabel -> DotColor
-- | Nullaryops
opDotColor (EmptyTableL _)   = Gray
opDotColor (LitTableL _ _)   = Gray
opDotColor (TableRefL _)     = Gray
-- | Unops
opDotColor (AggrL _)         = Gold
opDotColor (AttachL _)       = Gold
opDotColor (CastL _)         = Gold
opDotColor (DistinctL _)     = Gold
opDotColor (DummyL _)        = Gold
opDotColor (FunBoolNotL _)   = Gold
opDotColor (FunBinOpL _)     = Gold
opDotColor (ProjL _)         = Gold
opDotColor (RankL _)         = Gold
opDotColor (RowNumL _)       = Gold
opDotColor (RowRankL _)      = Gold
opDotColor (PosSelL _)       = Gold
opDotColor (SelL _)          = Gold
-- | Binops
opDotColor (CrossL     _)    = Crimson
opDotColor (DifferenceL _)   = Crimson
opDotColor (DisjUnionL _)    = Crimson
opDotColor (EqJoinL    _)    = Crimson
opDotColor (ThetaJoinL _)    = Crimson
opDotColor (SemiJoinL _)     = Crimson
opDotColor (AntiJoinL _)     = Crimson

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

-- |  Render a Dot document from the preamble, nodes and edges
renderDot :: [DotNode] -> [DotEdge] -> Doc
renderDot ns es = text "digraph" <> (braces $ preamble $$ nodeSection $$ edgeSection)
    where nodeSection = vcat $ map renderDotNode ns
          edgeSection = vcat $ map renderDotEdge es

-- | Labels (to collect all operations (nullary, unary,binary))
data PFLabel = EmptyTableL SchemaInfos
             | LitTableL SemInfLitTable SchemaInfos
             | TableRefL SemInfTableRef    -- nullops
             | AggrL SemInfAggr
             | AttachL SemInfAttach
             | CastL SemInfCast
             | DistinctL ()
             | DummyL String
             | FunBinOpL SemBinOp
             | FunBoolNotL SemUnOp
             | ProjL SemInfProj
             | RankL SemInfRank
             | RowNumL SemInfRowNum
             | RowRankL SemInfRank
             | SelL SemInfSel
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
labelOfUnOp (Aggr info) 		   	= AggrL info
labelOfUnOp (Attach info)	   	  = AttachL info
labelOfUnOp (Cast info) 		   	= CastL info
labelOfUnOp (Distinct info) 		= DistinctL  info
labelOfUnOp (Dummy info) 	   	  = DummyL info
labelOfUnOp (FunBinOp info) 		= FunBinOpL info
labelOfUnOp (FunBoolNot info) 	= FunBoolNotL info
labelOfUnOp (PosSel info)       = PosSelL info
labelOfUnOp (Proj info) 			  = ProjL info
labelOfUnOp (Rank info) 		   	= RankL info
labelOfUnOp (RowNum info) 	  	= RowNumL info
labelOfUnOp (RowRank info) 	  	= RowRankL info
labelOfUnOp (Sel info)          = SelL info

labelOfNullaryOp :: NullOp -> PFLabel
labelOfNullaryOp (EmptyTable  info)	      = EmptyTableL info
labelOfNullaryOp (LitTable  info1 info2)	= LitTableL info1 info2
labelOfNullaryOp (TableRef  info)      	  = TableRefL info

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
