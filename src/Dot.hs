-- Dot

module Dot() where

import qualified Data.IntMap                         		as Map
import           Data.List 							 		--as List

import           Text.PrettyPrint 					 		--as Pretty

import qualified Database.Algebra.Dag                		as Dag
import           Database.Algebra.Dag.Common		 		--as DagC
import           Database.Algebra.Pathfinder.Data.Algebra   as Alg
--import           Database.Algebra.X100.Render.Common


---------------- aus X100 Common / angepasst

-- Rendering projection information

nodeToDoc :: AlgNode -> Doc
nodeToDoc n = (text "id:") <+> (int n)

tagsToDoc :: [Tag] -> Doc
tagsToDoc ts = vcat $ map text ts


--labelToDoc :: AlgNode -> String -> Doc -> [Tag] -> Doc
--labelToDoc n s as ts = (nodeToDoc n) $$ ((text s) <> (parens as)) $$ (tagsToDoc $ nub ts)
labelToDoc :: AlgNode -> String -> [Tag] -> Doc
labelToDoc n s ts = (nodeToDoc n) $$ (text s) <> (tagsToDoc $ nub ts)

--renderJoinArgs :: [(ColID, ColID)] -> Doc
--renderJoinArgs cols =
--    let (k1, k2) = unzip cols
--    in bracketList renderColID k1 <> comma <+> bracketList renderColID k2

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

bracketList :: (a -> Doc) -> [a] -> Doc
bracketList f = brackets . hsep . punctuate comma . map f

bracketListLines :: (a -> Doc) -> [a] -> Doc
bracketListLines f = brackets . vcat . punctuate comma . map f

---------------------------------------------------

---- | Create an abstract Dot edge
--constructDotEdge :: (AlgNode, AlgNode) -> DotEdge
--constructDotEdge = uncurry DotEdge

-- create the node label from an operator description
--opDotLabel :: NodeMap [Tag] -> AlgNode -> PFLabel -> Doc
--opDotLabel tm i ProjL =
--  -- auskommentiert, da nicht mehr (ProjL info)
--  --let bl = if length info > 4
--  --         then bracketListLines
--  --         else bracketList
--     labelToDoc i "Project" (lookupTags i tm)
--opDotLabel tm i (AggrL (ks, as)) =
--     labelToDoc i "Aggr" ((bracketList renderColID ks) <> comma <+> (bracketList renderAggregation as)) (lookupTags i tm)
--opDotLabel tm i (OrdAggrL (ks, as)) =
--     labelToDoc i "OrdAggr" ((bracketList renderColID ks) <> comma <+> (bracketList renderAggregation as)) (lookupTags i tm)
--opDotLabel tm i (UnionL) =
--     labelToDoc i "Union" empty (lookupTags i tm)
--opDotLabel tm i (InlineLoadL (c, d)) =
--    labelToDoc i "InlineLoad"
--    (bracketList renderColumnName c <> comma
--     $$ renderData d <> comma
--     $$ quotes comma <> comma
--     <+> quotes semi <> comma)
--    (lookupTags i tm)
--opDotLabel tm i (SelectL info) =
--     labelToDoc i "Select" (renderExpr info) (lookupTags i tm)
--opDotLabel tm i (MergeUnionL info) =
--     labelToDoc i "MergeUnion" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (MergeDiffL info) =
--     labelToDoc i "MergeDiff" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (MergeJoin1L info) =
--     labelToDoc i "MergeJoin1" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (MergeJoinNL info) =
--     labelToDoc i "MergeJoinN" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (HashJoin01L info) =
--     labelToDoc i "HashJoin01" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (HashSemiJoinL info) =
--     labelToDoc i "HashSemiJoinL" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (HashJoin1L info) =
--     labelToDoc i "HashJoin1" (renderJoinArgs info) (lookupTags i tm)
--opDotLabel tm i (HashJoinNL (hashKeys, extraCond)) =
--     let args  = (renderJoinArgs hashKeys)
--         args' = case extraCond of
--                   Just e  -> args <+> renderExpr e
--                   Nothing -> args
--     in labelToDoc i "HashJoinN" args' (lookupTags i tm)
--opDotLabel tm i (StableSortL info) =
--     labelToDoc i "StableSort" (bracketList renderSortCol info) (lookupTags i tm)
--opDotLabel tm i (SortL info) =
--     labelToDoc i "Sort" (bracketList renderSortCol info) (lookupTags i tm)
--opDotLabel tm i (CartProdL (Just card)) =
--     labelToDoc i "Cartprod" (int card) (lookupTags i tm)
--opDotLabel tm i (CartProdL Nothing) =
--     labelToDoc i "Cartprod" empty (lookupTags i tm)
--opDotLabel tm i (FlowMatL) =  labelToDoc i "FlowMat" empty (lookupTags i tm)
--opDotLabel tm i (MScanL (t, cols, keys)) =
--  let bl = if length cols > 4
--           then bracketListLines
--           else bracketList
--  in labelToDoc i "MScan" (renderTableName t <> comma <+> bl renderScanColumn cols <> comma $$ renderTableKeys keys) (lookupTags i tm)
--opDotLabel tm i (AppendL info) =
--     labelToDoc i "Append" (renderTableName info) (lookupTags i tm)
--opDotLabel tm i NullOpL =
--     labelToDoc i "NullOp" empty (lookupTags i tm)
--opDotLabel tm i (ReuseL label) =
--     labelToDoc i "Reuse" (text label) (lookupTags i tm)



escapeLabel :: String -> String
escapeLabel s = concatMap escapeChar s

escapeChar :: Char -> [Char]
escapeChar '\n' = ['\\', 'n']
escapeChar '\\' = ['\\', '\\']
escapeChar '\"' = ['\\', '"']
escapeChar c = [c]



constructDotNode :: [AlgNode] -> NodeMap [Tag] -> (AlgNode, PFLabel) -> DotNode
constructDotNode rootNodes ts (n, op) =
    if elem n rootNodes then
        DotNode n l c (Just Solid)
    else
        DotNode n l c Nothing
    where l = "label"
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
labelOfOp (BinOp op _ _)	= labelOfBinOp op
labelOfOp (UnOp op _)		= labelOfUnOp op
labelOfOp (NullaryOp op)	= labelOfNullaryOp op

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

















--tagOfLink :: X100ReuseAlgebra -> Maybe [Tag]
--tagOfLink (BinOp _ (Label ll) (Label lr)) = Just ["Label: Left = " ++ ll ++ "Right = " ++ lr]
--tagOfLink (BinOp _ (Label l) _)           = Just ["Label: Left = " ++ l]
--tagOfLink (BinOp _ _ (Label l))           = Just ["Label: Right = " ++ l]
--tagOfLink (UnOp _ (Label l))              = Just ["Label: " ++ l]
--tagOfLink _                               = Nothing

--addLinkLabels :: NodeMap [Tag] -> NodeMap X100ReuseAlgebra -> NodeMap [Tag]
--addLinkLabels ts ops = Map.unionWith (++) ts $ Map.foldrWithKey (\n o m -> case tagOfLink o of
--                                                                    Just t -> Map.insert n t m
--                                                                    Nothing -> m) Map.empty ops

---- | Render an X100 algebra plan with Reuse operators into a dot file (GraphViz).
--renderX100ReuseDot :: NodeMap [Tag] -> [AlgNode] -> NodeMap X100ReuseAlgebra -> String
--renderX100ReuseDot ts roots m = render $ renderDot dotNodes dotEdges
--    where (opLabels, edges) = extractGraphStructure labelOfReuseOp d
--          d = Dag.mkDag m roots
--          ts' = addLinkLabels ts m
--          dotNodes = map (constructDotNode roots ts') opLabels
--          dotEdges = map constructDotEdge edges
