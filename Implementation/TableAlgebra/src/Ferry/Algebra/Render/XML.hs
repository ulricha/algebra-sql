{-# LANGUAGE TemplateHaskell #-}
module Ferry.Algebra.Render.XML where
{-
Transform a query plan DAG into an XML representation.
-}    
import Ferry.Impossible
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder
import Ferry.Algebra.Render.XMLUtils
import Control.Monad.Writer

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.Escape (xmlEscape)

import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M

-- Transform a query plan with result type into a pretty doc.
-- The type is used to add meta information to the XML that is used for pretty printing by ferryDB
transform :: (Bool, AlgPlan) -> Doc
transform (isList, p) = let plans = runXML M.empty $ planBuilder (mkProperty isList) p
                            planBundle = mkPlanBundle plans
                         in (document $ mkXMLDocument planBundle)

-- Transform a potentially nested algebraic plan into xml.
-- The first argument is the overall result type property of the query.
planBuilder :: Element () -> AlgPlan -> XML ()
planBuilder prop (nodes, (top, cols, subs)) = buildPlan Nothing (Just prop) (top, cols, subs)
    where
        buildPlan :: Maybe (Int, Int) -> Maybe (Element ()) -> AlgRes -> XML ()
        buildPlan parent props (top', cols', subs') = 
                                    do
                                        let colProp = cssToProp cols'
                                        let planProp = case props of
                                                        Nothing -> Elem "properties" [] [CElem colProp ()]
                                                        Just p  -> Elem "properties" [] [CElem colProp (), CElem p ()]
                                        let plan = runXML nodeTable $ serializeAlgebra top' cols'
                                        pId <- mkQueryPlan parent planProp plan
                                        buildSubPlans pId subs'
        buildSubPlans :: Int -> SubPlan -> XML ()
        buildSubPlans parent (SubPlan m) = let subPlans = M.toList m
                                            in mapM_ (\(cId, res) -> buildPlan (Just (parent, cId)) Nothing res) subPlans
        
        nodeTable = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList nodes


-- Convert columns structure to xml properties for rendering by ferry DB        
cssToProp :: Columns -> Element ()
cssToProp cols = Elem "property" [("name", AttValue [Left "cs"])] $ map (\x -> CElem (csToProp x) ()) cols

csToProp :: Column -> Element ()
csToProp (Col i ty) = Elem "property" [("name", AttValue [Left "offset"]), ("value", AttValue [Left $ show i])] 
                        [flip CElem () $ Elem "property" [("name", AttValue [Left "type"  ]), ("value", AttValue [Left $ show ty])] []]
csToProp (NCol x css) = Elem "property" [("name", AttValue [Left "mapping"]), ("value", AttValue [Left x])]
                            [CElem (cssToProp css) ()]

-- Serialize algebra
serializeAlgebra :: GraphNode -> Columns -> XML XMLNode
serializeAlgebra qGId cols = do
                                    qId <- alg2XML qGId
                                    nilId <- nilNode
                                    xId <- freshId
                                    let contentN = Elem "content" [] $ (:) (CElem iterCol ()) $ (:) (CElem posCol ()) $ map (\c -> CElem c ()) $ fst $ colsToNodes 1 cols 
                                    let edgeNil = mkEdge nilId
                                    let edgeQ = mkEdge qId
                                    tell [Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "serialize relation"])] [CElem contentN (), CElem edgeNil (), CElem edgeQ ()]]
                                    return xId

-- XML defintion of iter column
iterCol :: Element ()
iterCol = Elem "column" [("name", AttValue [Left "iter"]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "iter"])] []

-- XML defintion of position column
posCol :: Element ()
posCol = Elem "column" [("name", AttValue [Left "pos"]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "pos"])] []

-- Transform cs structure into xml columns
colsToNodes :: Int -> Columns -> ([Element ()], Int)
colsToNodes i ((Col n _):cs) = let col = Elem "column" [("name", AttValue [Left $ "item" ++ (show n)]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "item"]), ("position", AttValue [Left $ show i])] []
                                   (els, i') = colsToNodes (i+1) cs
                                in (col:els, i') 
colsToNodes i ((NCol _ cs):cs') = let (els, i') = colsToNodes i cs 
                                      (els', i'') = colsToNodes i' cs'
                                   in (els ++ els', i'')
colsToNodes i []                = ([], i)

-- XML defintion of nil node                                    
nilNode :: XML XMLNode
nilNode = do
            xId <- freshId
            tell [Elem "node" [("id", AttValue [Left $ show xId]),("kind", AttValue [Left "nil"])] []]
            return xId
            
-- Transform algebra into XML
-- The outer function determines whether the node was already translated into xml, if so it returns the xml id of that node.
-- if the node was not translated yet the inner function alg2XML' will translated the plan and return the xml id
alg2XML :: GraphNode -> XML XMLNode 
alg2XML gId = do
                def <- isDefined gId
                case def of
                    Just x -> return x
                    Nothing -> do
                                nd <- getNode gId
                                xId <- alg2XML' nd
                                addNodeTrans gId xId
                                return xId
                
                
 where
    alg2XML' :: Algebra -> XML XMLNode 
    alg2XML' (LitTable [[v]] [(n, ty)]) = do
                                            xId <- freshId
                                            tell [mkTableNode xId n v ty]
                                            return xId 
    alg2XML' (Attach (n, (ty, val)) cId1) = do
                                                cxId1 <- alg2XML cId1
                                                xId <- freshId
                                                tell [mkAttachNode xId n val ty cxId1]
                                                return xId
    alg2XML' (Proj proj cId1) = do
                                    cxId1 <- alg2XML cId1
                                    xId <- freshId
                                    tell [mkProjNode xId proj cxId1]
                                    return xId
    alg2XML' (EqJoin jc cId1 cId2) = do
                                                cxId1 <- alg2XML cId1
                                                cxId2 <- alg2XML cId2
                                                xId <- freshId
                                                tell [mkEqJoinNode xId jc cxId1 cxId2]
                                                return xId
    alg2XML' (FunBinOp (op, res, lArg, rArg) cId) = do
                                                        cxId1 <- alg2XML cId
                                                        xId <- freshId
                                                        tell [mkBinOpNode xId op res lArg rArg cxId1]
                                                        return xId
    alg2XML' (EmptyTable schema) = do
                                         xId <- freshId
                                         tell [mkEmptyTable xId schema]
                                         return xId
    alg2XML' (DisjUnion cId1 cId2) = do
                                          cxId1 <- alg2XML cId1
                                          cxId2 <- alg2XML cId2
                                          xId <- freshId
                                          tell [mkUnion xId cxId1 cxId2]
                                          return xId
    alg2XML' (Rank (res, sort) cId1) = do
                                            cxId1 <- alg2XML cId1
                                            xId <- freshId
                                            tell [mkRank xId res sort cxId1]
                                            return xId
    alg2XML' (Cross cId1 cId2) = do
                                        cxId1 <- alg2XML cId1
                                        cxId2 <- alg2XML cId2
                                        xId <- freshId
                                        tell [mkCross xId cxId1 cxId2]
                                        return xId
    alg2XML' (TableRef (n, cs, ks)) = do
                                            xId <- freshId
                                            tell [mkTable xId n cs ks]
                                            return xId
    alg2XML' (Sel n cId1) = do
                                cxId <- alg2XML cId1
                                xId <- freshId
                                tell [mkSelect xId n cxId]
                                return xId
    alg2XML' (PosSel (n, sort, part) cId1) = do
                                                  cxId1 <- alg2XML cId1
                                                  xId <- freshId
                                                  tell [mkPosSel xId n sort part cxId1]
                                                  return xId
    alg2XML' (FunBoolNot (res, col) cId1) = do
                                                 cxId1 <- alg2XML cId1
                                                 xId <- freshId
                                                 tell [mkBoolNot xId res col cxId1]
                                                 return xId
    alg2XML' (RowNum (res, sort, part) cId1) = do
                                                    cxId1 <- alg2XML cId1
                                                    xId <- freshId
                                                    tell [mkRowNum xId res sort part cxId1]
                                                    return xId
    alg2XML' (Distinct cId1) = do
                                    cxId <- alg2XML cId1
                                    xId <- freshId
                                    tell [mkDistinct xId cxId]
                                    return xId
    alg2XML' (RowRank (res, sort) cId1) = do
                                              cxId1 <- alg2XML cId1
                                              xId <- freshId
                                              tell [mkRowRank xId res sort cxId1]
                                              return xId
    alg2XML' (Aggr (aggrs, part) cId1)
                            = do
                                cxId1 <- alg2XML cId1
                                xId <- freshId
                                tell [mkAggrs xId aggrs part cxId1]
                                return xId
    alg2XML' (Cast (r, o, t) cId1) = do
                                        cxId1 <- alg2XML cId1
                                        xId <- freshId
                                        tell [mkCast xId o r t cxId1]
                                        return xId
    alg2XML' (Difference cId1 cId2) = do
                                        cxId1 <- alg2XML cId1
                                        cxId2 <- alg2XML cId2
                                        xId <- freshId
                                        tell [mkDifference xId cxId1 cxId2]
                                        return xId
    alg2XML' _ = $impossible

mkDifference :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkDifference xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "difference" 

mkCast :: XMLNode -> AttrName -> AttrName -> ATy -> XMLNode -> Element ()
mkCast xId o r t c = [[column r True, column o False, typeN t] `childsOf` contentNode, mkEdge c] `childsOf` node xId "cast"

mkAggrs :: XMLNode -> [(AggrType, ResAttrName, Maybe AttrName)] -> Maybe PartAttrName -> XMLNode -> Element ()
mkAggrs xId aggrs part cId = let partCol = case part of
                                            Nothing -> []
                                            Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                 aggr = map mkAggr aggrs 
                              in [(partCol ++ aggr) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "aggr"  
    where
        mkAggr :: (AggrType, ResAttrName, Maybe AttrName) -> Element ()
        mkAggr (aggr, res, arg) = let argCol = case arg of
                                                    Just arg' -> [[attr "function" "item"] `attrsOf` column arg' False]
                                                    Nothing -> [] 
                                   in ((column res True):argCol) `childsOf` [attr "kind" $ show aggr] `attrsOf` xmlElem "aggregate"

mkPosSel :: XMLNode -> Int -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkPosSel xId n sort part cId = let sortCols = map mkSortColumn $ zip sort [1..]
                                   partCol = case part of
                                                   Nothing -> []
                                                   Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                   posNode = n `dataChildOf` xmlElem "position" 
                                in [((posNode:sortCols) ++ partCol) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "pos_select" 

-- Create an xml rank element node. 
mkRowRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRowRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                              in [(column res True : sortCols) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "rowrank"

-- | Create an xml distinct node
mkDistinct :: XMLNode -> XMLNode -> Element ()
mkDistinct xId cxId = [mkEdge cxId] `childsOf` node xId "distinct" 

-- | Create an xml rownum node                                                    
mkRowNum :: XMLNode -> ResAttrName -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkRowNum xId res sort part cxId = let sortCols = map mkSortColumn $ zip sort [1..]
                                      partCol = case part of
                                                    Nothing -> []
                                                    Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                   in [(column res True:(sortCols ++ partCol)) `childsOf` contentNode , mkEdge cxId] `childsOf` node xId "rownum"

-- | Create an xml boolean not node           
mkBoolNot :: XMLNode -> String -> String -> XMLNode -> Element ()
mkBoolNot xId res arg cxId = [[column res True, column arg False] `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "not"

-- | Create an xml select node
mkSelect :: XMLNode -> String -> XMLNode -> Element ()
mkSelect xId n cxId = [[column n False] `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "select"

-- | Create an xml table binding node
mkTable :: XMLNode -> String -> TableAttrInf -> KeyInfos -> Element ()
mkTable xId n descr keys = [[mkKeys keys] `childsOf` xmlElem "properties", [mkTableDescr n descr] `childsOf` contentNode] `childsOf` node xId "ref_tbl"
                                           
-- | Create an xml table description node
mkTableDescr :: String -> TableAttrInf -> Element ()
mkTableDescr n descr = map (\d -> toTableCol d ) descr `childsOf` [attr "name" $ "&quot;" ++n ++ "&quot;"] `attrsOf` xmlElem "table"
    where
     toTableCol :: (AttrName, AttrName, ATy) -> Element ()
     toTableCol (cn, xn, t) = [attr "name" xn, attr "tname" $ "&quot;" ++ cn ++ "&quot;", attr "type" $ show t] `attrsOf` xmlElem "column"

-- | Create an xml table key node
mkKey :: KeyInfo -> Element ()
mkKey k = let bd = map (\(k', p) -> [attr "name" k', attr "position" $ show p] `attrsOf` xmlElem "column") $ zip k [1..]
           in bd `childsOf` xmlElem "key" 

-- | Create an xml node containing multiple table keys           
mkKeys :: KeyInfos -> Element ()
mkKeys ks = map mkKey ks `childsOf` xmlElem "keys"
             
-- Create an xml rank element node. 
mkRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                              resCol = column res True
                           in [resCol:sortCols `childsOf` contentNode, mkEdge cId] `childsOf` node xId "rank" 

-- Create an xml sort column node for use in the rank node.    
mkSortColumn :: ((SortAttrName, SortDir), Int) -> Element ()
mkSortColumn ((n, d), p) = [attr "function" "sort", attr "position" $ show p, attr "direction" $ show d] `attrsOf` column n False


-- Create an xml cross node
mkCross :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkCross xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "cross"

-- Create an xml union node                                          
mkUnion :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkUnion xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "union" 

-- Create an empty table node, table needs to contain type information
mkEmptyTable :: XMLNode -> SchemaInfos -> Element ()
mkEmptyTable xId schema = [map mkColumn schema `childsOf` contentNode] `childsOf` node xId "empty_tbl"

-- Create an xml column node
mkColumn :: (AttrName, ATy) -> Element ()
mkColumn (n, t) = [attr "type" $ show t] `attrsOf` column n True

-- Create an xml binary operator node.
-- Three sort of binary operators exist:
--  1. Arithmatic operators, represented in xml as function nodes
--  2. Relational operators, represented in xml as relational function nodes
--  3. Operators that can be expressed in terms of other operators                                                            
mkBinOpNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkBinOpNode xId op res lArg rArg cId | elem op ["+", "-", "*", "%", "/"] = mkFnNode xId (arOptoFn op) res lArg rArg cId
                                     | elem op [">", "==", "and", "or", "&&", "||"] = mkRelFnNode xId (relOptoFn op) res lArg rArg cId
                                     | elem op ["<" ] = mkBinOpNode xId ">" res rArg lArg cId
                                     | otherwise = $impossible
        where
            arOptoFn :: String -> String
            arOptoFn "+" = "add"
            arOptoFn "-" = "subtract"
            arOptoFn "/" = "divide"
            arOptoFn "*" = "multiply"
            arOptoFn "%" = "modulo"
            arOptoFn _ = $impossible
            relOptoFn :: String -> String
            relOptoFn ">" = "gt"
            relOptoFn "==" = "eq"
            relOptoFn "and" = "and"
            relOptoFn "or" = "or"
            relOptoFn "&&" = "and"
            relOptoFn "||" = "or"
            relOptoFn _ = $impossible

-- Create an XML relational function node
mkRelFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkRelFnNode xId fn res lArg rArg cId = let content = [column res True,
                                                      [attr "position" "1"] `attrsOf` column lArg False,
                                                      [attr "position" "2"] `attrsOf` column rArg False] `childsOf` contentNode
                                        in [content, mkEdge cId] `childsOf` node xId fn
                                                       

-- Create an XML function node            
mkFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkFnNode xId fn res lArg rArg cId = let cont = [[attr "name" fn] `attrsOf` xmlElem "kind",
                                                column res True,
                                                [attr "position" "1"] `attrsOf` column lArg False, 
                                                [attr "position" "2"] `attrsOf` column rArg False] `childsOf` contentNode
                                     in [cont, mkEdge cId] `childsOf` node xId "fun"

-- Create an XML eq-join node.             
mkEqJoinNode :: XMLNode -> (LeftAttrName,RightAttrName) -> XMLNode -> XMLNode -> Element ()
mkEqJoinNode xId (lN, rN) cxId1 cxId2 = let contNode = [[attr "position" "1"] `attrsOf` column lN False,
                                                        [attr "position" "2"] `attrsOf` column rN False] `childsOf` contentNode
                                         in [contNode, mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "eqjoin"
                                                        
-- Create an XML projection node
mkProjNode :: XMLNode -> [(NewAttrName, OldAttrName)] -> XMLNode -> Element ()
mkProjNode xId mapping cxId = [map mkProjColumn mapping `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "project"
    where
      mkProjColumn :: (NewAttrName, OldAttrName) -> Element ()
      mkProjColumn (n, o) = [attr "old_name" o] `attrsOf` column n True

-- Create an xml attach column node 
mkAttachNode :: XMLNode -> ColName -> AVal -> ATy -> XMLNode -> Element ()
mkAttachNode xId n val ty cxId = let valNode = val `dataChildOf` [attr "type" $ show ty] `attrsOf` xmlElem "value"  
                                     colNode = [xmlEscape xmlEscaper valNode] `childsOf` column n True
                                  in [[colNode] `childsOf` contentNode, mkEdge cxId]`childsOf` node xId "attach"

-- Create an xml table node with one value in it
mkTableNode :: XMLNode -> ColName -> AVal -> ATy -> Element ()
mkTableNode xId n val ty = let valNode = val `dataChildOf` [attr "type" $ show ty] `attrsOf` xmlElem "value"
                               colNode = [xmlEscape xmlEscaper valNode] `childsOf` column n True
                               conNode =  [colNode] `childsOf` contentNode
                            in [conNode] `childsOf` node xId "table"

-- Create an xml edge to point to the given xml node id.
mkEdge :: XMLNode -> Element ()
mkEdge n = [attr "to" $ show n] `attrsOf` xmlElem "edge"

-- Transform the given plan nodes into an xml query plan.
-- The first argument can contain additional property node information
mkQueryPlan :: Maybe (Int, Int) -> Element () -> [Element ()] -> XML Int
mkQueryPlan parent props els = let logicalPlan = els `childsOf` [attr "unique_names" "true"] `attrsOf` xmlElem "logical_query_plan"
                         in do
                             planId <- freshId
                             let attrs = case parent of
                                            Nothing -> [attr "id" $ show planId]
                                            Just (p, c) -> [attr "id" $ show planId, attr "idref" $ show p, attr "colref" $ show c]
                             tell [[props, logicalPlan] `childsOf` attrs `attrsOf` xmlElem "query_plan"]
                             return planId
                        
-- Create a plan bundle out of the given query plans
mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = plans `childsOf` xmlElem "query_plan_bundle"

-- Create an xml document out of the given root tag.
mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prol = Prolog (Just xmlDecl) [] Nothing []
                    in Document prol emptyST el []

-- Create an xml property node so that ferryDB knows more or less how to print the result
mkProperty :: Bool -> Element ()
mkProperty isList = [attr "name" "overallResultType", attr "value" result] `attrsOf` xmlElem "property"
    where
        result = case isList of
                    True  -> "LIST"
                    False -> "TUPLE"
