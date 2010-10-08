{-# LANGUAGE TemplateHaskell #-}
module Ferry.Algebra.Render.XML where
{-
Transform a query plan DAG into an XML representation.
-}    
import Ferry.Impossible
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.Escape (xmlEscapeContent, mkXmlEscaper, XmlEscaper ())

import Ferry.TypedCore.Data.Type (FType (..), Qual (..))

import Data.Char (ord)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader


import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M

-- Convenient alias for column names
type ColName = String

-- The Graph is represented as a tuple of an int, that represents the first node, and
-- a list of algebraic nodes with their node numbers.
type Graph = (AlgNode, [(Algebra, AlgNode)])

-- Alias for GraphNode ids
type GraphNode = Int
-- Alias for xmlNode ids
type XMLNode = Int

-- Mapping from graphnodes to xmlnode ids. This dictionary is used to prevent duplicate xml nodes
type Dictionary = M.Map GraphNode XMLNode

-- XML monad, all elements are printed in bottom up!!! order into the writer monad so
-- that the xml can easily be printed an will be accepted by pfopt.
-- The reader monad contains the map with all the nodes from the algebraic plan, the keys
-- are the node ids from the graph. The state monad keeps track of the supply of fresh ids
-- for xml nodes and the dictionary for looking up whether a certain graphnode already has
-- an xml representation.
type XML = WriterT [Element ()] (ReaderT (M.Map AlgNode Algebra) (State (Int, Dictionary)))

-- Has a graphnode already been translated into an xml node. If yes which node?
isDefined :: GraphNode -> XML (Maybe XMLNode)
isDefined g = do
                (_, d) <- get
                return $ M.lookup g d 

-- Get a fresh xml node id.
freshId :: XML Int
freshId = do
            (n, d) <- get
            put (n + 1, d)
            return n

-- Add a mapping from a graphnode to an xml node id to the dictionary            
addNodeTrans :: GraphNode -> XMLNode -> XML ()
addNodeTrans gId xId = do
                        (n, d) <- get
                        put (n, M.insert gId xId d)

-- Get a node from the algebraic plan with a certain graphNode id number
getNode :: Int -> XML Algebra
getNode i = do
             nodes <- ask
             return $ nodes M.! i


-- Run the monad and return a list of xml elements from the monad.
runXML :: M.Map AlgNode Algebra -> XML a -> [Element ()]
runXML m = snd . fst . flip runState (0, M.empty) . flip runReaderT m . runWriterT 

-- Transform a query plan with result type into a pretty doc.
-- The type is used to add meta information to the XML that is used for pretty printing by ferryDB
transform :: (Qual FType, AlgPlan) -> Doc
transform (_ :=> t, p) = let plans = runXML M.empty $ planBuilder (mkProperty t) p
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
                                node <- getNode gId
                                xId <- alg2XML' node
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
    alg2XML' _ = $impossible

mkAggrs :: XMLNode -> [(AggrType, ResAttrName, Maybe AttrName)] -> Maybe PartAttrName -> XMLNode -> Element ()
mkAggrs xId aggrs part cId = let partCol = case part of
                                            Nothing -> []
                                            Just x  -> [Elem "column" [("name", AttValue [Left x]),("function", AttValue [Left "partition"]),("new", AttValue [Left "false"])] []]
                                 aggr = map mkAggr aggrs
                                 contNodes = map (\x -> CElem x ()) (partCol ++ aggr)
                                 edge = mkEdge cId
                                 contents = [flip CElem () $ Elem "content" [] contNodes, CElem edge ()] 
                              in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "aggr"])] contents
    where
        mkAggr :: (AggrType, ResAttrName, Maybe AttrName) -> Element ()
        mkAggr (aggr, res, arg) = let argCol = case arg of
                                                    Just arg' -> [flip CElem () $ Elem "column" [("name", AttValue [Left arg']), ("new", AttValue [Left "false"]), ("function", AttValue [Left "item"])] []]
                                                    Nothing -> [] 
                                   in Elem "aggregate" [("kind", AttValue [Left $ show aggr])] ((flip CElem () $ Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []):argCol)
        


mkPosSel :: XMLNode -> Int -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkPosSel xId n sort part cId = let sortCols = map mkSortColumn $ zip sort [1..]
                                   partCol = case part of
                                                   Nothing -> []
                                                   Just x  -> [Elem "column" [("name", AttValue [Left x]),("function", AttValue [Left "partition"]),("new", AttValue [Left "false"])] []]
                                   posNode = Elem "position" [] [CString False (show n) ()]
                                   contNode = contentsNode ((posNode:sortCols) ++ partCol)
                                   edge = mkEdge cId
                                in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "pos_select"])] [CElem contNode (), CElem edge ()]

-- Create an xml rank element node. 
mkRowRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRowRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                                 resCol = Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []
                                 contNode = contentsNode (resCol:sortCols)
                                 edge = mkEdge cId
                              in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "rowrank"])] [CElem contNode (), CElem edge ()]


-- | Create an xml distinct node
mkDistinct :: XMLNode -> XMLNode -> Element ()
mkDistinct xId cxId = let edge1 = mkEdge cxId
                       in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "distinct"])] [CElem edge1 ()]

-- | Create an xml rownum node                                                    
mkRowNum :: XMLNode -> ResAttrName -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkRowNum xId res sort part cxId = let sortCols = map mkSortColumn $ zip sort [1..]
                                      resCol = Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []
                                      partCol = case part of
                                                    Nothing -> []
                                                    Just x  -> [Elem "column" [("name", AttValue [Left x]),("function", AttValue [Left "partition"]),("new", AttValue [Left "false"])] []]
                                      contNode = Elem "content" [] $ map (\x -> CElem x ()) $ resCol:(sortCols ++ partCol)
                                      edge = mkEdge cxId
                                   in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "rownum"])]
                                                  [CElem contNode (), CElem edge ()]
                                                    
-- | Create an xml boolean not node           
mkBoolNot :: XMLNode -> String -> String -> XMLNode -> Element ()
mkBoolNot xId res arg cxId = let resCol = Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []
                                 argCol = Elem "column" [("name", AttValue [Left arg]), ("new", AttValue [Left "false"])] []
                                 cont = Elem "content" [] [CElem resCol (), CElem argCol ()]
                                 edge = mkEdge cxId
                              in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "not"])]
                                             [CElem cont (), CElem edge ()]

-- | Create an xml select node
mkSelect :: XMLNode -> String -> XMLNode -> Element ()
mkSelect xId n cxId = let col = Elem "column" [("name", AttValue [Left n]), ("new", AttValue [Left "false"])] []
                          cont = Elem "content" [] [CElem col ()]
                          edge = mkEdge cxId
                       in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "select"])]
                                      [CElem cont (), CElem edge ()]

-- | Create an xml table binding node
mkTable :: XMLNode -> String -> TableAttrInf -> KeyInfos -> Element ()
mkTable xId n descr keys = let props = Elem "properties" [] [flip CElem () $ mkKeys keys ]
                               cont = Elem "content" [] [flip CElem () $ mkTableDescr n descr]
                            in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "ref_tbl"])]
                                           [CElem props (), CElem cont ()]

-- | Create an xml table description node
mkTableDescr :: String -> TableAttrInf -> Element ()
mkTableDescr n descr = Elem "table" [("name", AttValue [Left $ "&quot;" ++n ++ "&quot;"])] $ map (\d -> flip CElem () $ toTableCol d ) descr
    where
     toTableCol :: (AttrName, AttrName, ATy) -> Element ()
     toTableCol (cn, xn, t) = Elem "column" [("name", AttValue [Left xn]), ("tname", AttValue [Left $ "&quot;" ++ cn ++ "&quot;"]), ("type", AttValue [Left $ show t])] []

-- | Create an xml table key node
mkKey :: KeyInfo -> Element ()
mkKey k = let bd = map (\(k', p) -> CElem (Elem "column" [("name", AttValue [Left k']), ("position", AttValue [Left $ show p])] []) ()) $ zip k [1..]
           in Elem "key" [] bd

-- | Create an xml node containing multiple table keys           
mkKeys :: KeyInfos -> Element ()
mkKeys ks = let bd = map (\k -> CElem (mkKey k) ()) ks
             in Elem "keys" [] bd
             
-- Create an xml rank element node. 
mkRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                              resCol = Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []
                              contNode = contentsNode (resCol:sortCols)
                              edge = mkEdge cId
                           in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "rank"])] [CElem contNode (), CElem edge ()]

-- Create an xml sort column node for use in the rank node.    
mkSortColumn :: ((SortAttrName, SortDir), Int) -> Element ()
mkSortColumn ((n, d), p) = Elem "column" [("name", AttValue [Left n]),
                                          ("function", AttValue [Left "sort"]),
                                          ("position", AttValue [Left $ show p]),
                                          ("direction", AttValue [Left $ show d]),
                                          ("new", AttValue [Left "false"])] []

-- Create an xml cross node
mkCross :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkCross xId cxId1 cxId2 = let edge1 = mkEdge cxId1
                              edge2 = mkEdge cxId2
                           in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "cross"])] [CElem edge1 (), CElem edge2 ()]

-- Create an xml union node                                          
mkUnion :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkUnion xId cxId1 cxId2 = let edge1 = mkEdge cxId1
                              edge2 = mkEdge cxId2
                           in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "union"])] [CElem edge1 (), CElem edge2 ()]

-- Create an empty table node, table needs to contain type information
mkEmptyTable :: XMLNode -> SchemaInfos -> Element ()
mkEmptyTable xId schema = let contNode = contentsNode $ map mkColumn schema
                           in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "empty_tbl"])] [CElem contNode ()]

-- Create an xml column node
mkColumn :: (AttrName, ATy) -> Element ()
mkColumn (n, t) = Elem "column" [("name", AttValue [Left n]), ("type", AttValue [Left $ show t]),("new", AttValue [Left "true"])] []

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
mkRelFnNode xId fn res lArg rArg cId = let content = Elem "content" [] [CElem (Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []) (),
                                                                        CElem (Elem "column" [("name", AttValue [Left lArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) (),
                                                                        CElem (Elem "column" [("name", AttValue [Left rArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                           edge = mkEdge cId
                                        in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left fn])]
                                                       [CElem content (), CElem edge ()]

-- Create an XML function node            
mkFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkFnNode xId fn res lArg rArg cId = let kNode = Elem "kind" [("name", AttValue [Left fn])] []
                                        cont = Elem "content" [] [CElem kNode (),
                                                                     CElem (Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left lArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left rArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                        edge = mkEdge cId
                                     in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "fun"])]
                                                    [CElem cont (), CElem edge ()]

-- Create an XML eq-join node.             
mkEqJoinNode :: XMLNode -> (LeftAttrName,RightAttrName) -> XMLNode -> XMLNode -> Element ()
mkEqJoinNode xId (lN, rN) cxId1 cxId2 = let contNode = Elem "content" [] [CElem (Elem "column" [("name", AttValue [Left lN]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) ()
                                                                         ,CElem (Elem "column" [("name", AttValue [Left rN]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                            edge1 = mkEdge cxId1
                                            edge2 = mkEdge cxId2
                                         in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "eqjoin"])]
                                                        [CElem contNode (), CElem edge1 (), CElem edge2 ()]
                                                        
-- Create an XML projection node
mkProjNode :: XMLNode -> [(NewAttrName, OldAttrName)] -> XMLNode -> Element ()
mkProjNode xId mapping cxId = let contNode = Elem "content" [] $ map (\m -> flip CElem () $ mkProjColumn m) mapping
                                  edgeNode = mkEdge cxId
                               in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "project"])]
                                              [CElem contNode (), CElem edgeNode ()]
    where
      mkProjColumn :: (NewAttrName, OldAttrName) -> Element ()
      mkProjColumn (n, o) = Elem "column" [("name", AttValue [Left n]), ("old_name", AttValue [Left o]), ("new", AttValue [Left "true"])] []

-- Create an xml attach column node 
mkAttachNode :: XMLNode -> ColName -> AVal -> ATy -> XMLNode -> Element ()
mkAttachNode xId n val ty cxId = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                                     colNode = Elem "column" [("name", AttValue [Left n])
                                                             ,("new", AttValue [Left "true"])]
                                                             (xmlEscapeContent xmlEscaper [CElem valNode ()])
                                     conNode = contentNode colNode
                                     edgeNode = mkEdge cxId
                                  in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "attach"])] [CElem conNode (), CElem edgeNode ()]

-- Create an xml table node with one value in it
mkTableNode :: XMLNode -> ColName -> AVal -> ATy -> Element ()
mkTableNode xId n val ty = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                               colNode = Elem "column" [("name", AttValue [Left n])
                                                       ,("new", AttValue [Left "true"])]
                                                       (xmlEscapeContent xmlEscaper [CElem valNode ()])
                               conNode = contentNode colNode
                            in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "table"])] [CElem conNode ()] 

-- Create an xml edge to point to the given xml node id.
mkEdge :: XMLNode -> Element ()
mkEdge n = Elem "edge" [("to", AttValue [Left $ show n])] []

-- Wrap the given element in an xml content node
contentNode :: Element () -> Element ()
contentNode n = Elem "content" [] [CElem n ()]

-- Wrap the given elements in an xml content node
contentsNode :: [Element ()] -> Element ()
contentsNode ns = Elem "content" [] $ map (\n -> CElem n ()) ns

-- Transform the given plan nodes into an xml query plan.
-- The first argument can contain additional property node information
mkQueryPlan :: Maybe (Int, Int) -> Element () -> [Element ()] -> XML Int
mkQueryPlan parent props els = let logicalPlan = Elem "logical_query_plan" [("unique_names", AttValue [Left "true"])] $ map (\n -> CElem n ()) els
                                   propPlan = [CElem props ()]
                         in do
                             planId <- freshId
                             let attrs = case parent of
                                            Nothing -> [("id", AttValue [Left $ show planId])]
                                            Just (p, c) -> [("id", AttValue [Left $ show planId]), ("idref", AttValue [Left $ show p]), ("colref", AttValue [Left $ show c])]
                             tell $ [Elem "query_plan" attrs $ propPlan ++ [CElem logicalPlan ()]]
                             return planId
                        
-- Create a plan bundle out of the given query plans
mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = Elem "query_plan_bundle" [] $ map (\p -> CElem p ()) plans

-- Create an xml document out of the given root tag.
mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prol = Prolog (Just xmlDecl) [] Nothing []
                    in Document prol emptyST el []

-- Create an xml property node so that ferryDB knows more or less how to print the result
mkProperty :: FType -> Element ()
mkProperty ty = Elem "property" [("name", AttValue [Left "overallResultType"]), ("value", AttValue[Left result])] []
    where
        result = case ty of
                    FList _ -> "LIST"
                    _       -> "TUPLE"
                    
xmlEscaper :: XmlEscaper
xmlEscaper = mkXmlEscaper
   [('\60',"lt"),('\62',"gt"),('\38',"amp"),('\39',"apos"),('\34',"quot"), ('\92', "\\")]
   (\ ch ->
      let
         i = ord ch
      in
         i < 10 || (10<i && i<32) || i >= 127 ||
            case ch of
               '\'' -> True
               '\"' -> True
               '&' -> True
               '<' -> True
               '>' -> True
               '\\' -> True 
               _ -> False
      )
