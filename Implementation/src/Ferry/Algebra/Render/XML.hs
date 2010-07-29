module Ferry.Algebra.Render.XML where
{-
Transform a query plan DAG into an XML representation.
-}    
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty

import Ferry.TypedCore.Data.Type

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad


import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M

-- Convenient alias for column names
type ColName = String

-- The Graph is represented as a tuple of an int, that represents the first node, and
-- a list of algebraic nodes with their node numbers.
type Graph = (AlgNode, [(AlgConstr, AlgNode)])

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
type XML = WriterT [Element ()] (ReaderT (M.Map AlgNode AlgConstr) (State (Int, Dictionary)))

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
getNode :: Int -> XML AlgConstr
getNode i = do
             nodes <- ask
             return $ nodes M.! i


-- Run the monad and return a list of xml elements from the monad.
runXML :: M.Map AlgNode AlgConstr -> XML a -> [Element ()]
runXML m = snd . fst . flip runState (0, M.empty) . flip runReaderT m . runWriterT 

-- Transform a query plan with result type into a pretty doc.
-- The type is used to add meta information to the XML that is used for pretty printing by ferryDB
transform :: (Qual FType, AlgPlan) -> Doc
transform (_ :=> t, (nodes, (top, cols, subs))) = let nodeTable = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList nodes
                                                      plan = runXML nodeTable $ serializeAlgebra top cols
                                                      qPlan = runXML M.empty $ mkQueryPlan (Just $ mkProperties t) plan
                                                      planBundle = mkPlanBundle qPlan
                                                      props = mkProperties t
                                                   in (document $ mkXMLDocument planBundle)

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
colsToNodes i ((NCol n cs):cs') = let (els, i') = colsToNodes i cs 
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
    alg2XML' :: AlgConstr -> XML XMLNode 
    alg2XML' (LitTable [[v]] [(n, ty)], _) = do
                                            xId <- freshId
                                            tell [mkTableNode xId n v ty]
                                            return xId 
    alg2XML' (Attach (n, (ty, val)),[cId1]) = do
                                                cxId1 <- alg2XML cId1
                                                xId <- freshId
                                                tell [mkAttachNode xId n val ty cxId1]
                                                return xId
    alg2XML' (Proj proj, [cId1]) = do
                                    cxId1 <- alg2XML cId1
                                    xId <- freshId
                                    tell [mkProjNode xId proj cxId1]
                                    return xId
    alg2XML' (EqJoin jc, [cId1, cId2]) = do
                                                cxId1 <- alg2XML cId1
                                                cxId2 <- alg2XML cId2
                                                xId <- freshId
                                                tell [mkEqJoinNode xId jc cxId1 cxId2]
                                                return xId
    alg2XML' (FunBinOp (op, res, lArg, rArg), [cId]) = do
                                                        cxId1 <- alg2XML cId
                                                        xId <- freshId
                                                        tell [mkBinOpNode xId op res lArg rArg cxId1]
                                                        return xId
    alg2XML' (EmptyTable schema, []) = do
                                         xId <- freshId
                                         tell [mkEmptyTable xId schema]
                                         return xId
    alg2XML' (DisjUnion, [cId1, cId2]) = do
                                          cxId1 <- alg2XML cId1
                                          cxId2 <- alg2XML cId2
                                          xId <- freshId
                                          tell [mkUnion xId cxId1 cxId2]
                                          return xId
    alg2XML' (Rank (res, sort), [cId1]) = do
                                            cxId1 <- alg2XML cId1
                                            xId <- freshId
                                            tell [mkRank xId res sort cxId1]
                                            return xId
    alg2XML' (Cross, [cId1, cId2]) = do
                                        cxId1 <- alg2XML cId1
                                        cxId2 <- alg2XML cId2
                                        xId <- freshId
                                        tell [mkCross xId cxId1 cxId2]
                                        return xId
    alg2XML' (TableRef (n, cs, ks), []) = do
                                            xId <- freshId
                                            tell [mkTable xId n cs ks]
                                            return xId
    alg2XML' (Sel n, [cId1]) = do
                                cxId <- alg2XML cId1
                                xId <- freshId
                                tell [mkSelect xId n cxId]
                                return xId
    alg2XML' (FunBoolNot (res, col), [cId1]) = do
                                                 cxId1 <- alg2XML cId1
                                                 xId <- freshId
                                                 tell [mkBoolNot xId res col cxId1]
                                                 return xId
                                
mkBoolNot :: XMLNode -> String -> String -> XMLNode -> Element ()
mkBoolNot xId res arg cxId = let resCol = Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []
                                 argCol = Elem "column" [("name", AttValue [Left arg]), ("new", AttValue [Left "false"])] []
                                 cont = Elem "content" [] [CElem resCol (), CElem argCol ()]
                                 edge = mkEdge cxId
                              in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "not"])]
                                             [CElem cont (), CElem edge ()]

mkSelect :: XMLNode -> String -> XMLNode -> Element ()
mkSelect xId n cxId = let col = Elem "column" [("name", AttValue [Left n]), ("new", AttValue [Left "false"])] []
                          cont = Elem "content" [] [CElem col ()]
                          edge = mkEdge cxId
                       in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "select"])]
                                      [CElem cont (), CElem edge ()]

mkTable :: XMLNode -> String -> TableAttrInf -> KeyInfos -> Element ()
mkTable xId n descr keys = let props = Elem "properties" [] [flip CElem () $ mkKeys keys ]
                               cont = Elem "content" [] [flip CElem () $ mkTableDescr n descr]
                            in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "ref_tbl"])]
                                           [CElem props (), CElem cont ()]

mkTableDescr :: String -> TableAttrInf -> Element ()
mkTableDescr n descr = Elem "table" [("name", AttValue [Left n])] $ map (\d -> flip CElem () $ toTableCol d ) descr
    where
     toTableCol :: (AttrName, AttrName, ATy) -> Element ()
     toTableCol (cn, xn, t) = Elem "column" [("name", AttValue [Left xn]), ("tname", AttValue [Left cn]), ("type", AttValue [Left $ show t])] []

mkKey :: KeyInfo -> Element ()
mkKey k = let bd = map (\(k, p) -> CElem (Elem "column" [("name", AttValue [Left k]), ("position", AttValue [Left $ show p])] []) ()) $ zip k [1..]
           in Elem "key" [] bd
           
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
        where
            arOptoFn :: String -> String
            arOptoFn "+" = "add"
            arOptoFn "-" = "subtract"
            arOptoFn "/" = "divide"
            arOptoFn "*" = "multiply"
            arOptoFn "%" = "modulo"
            relOptoFn :: String -> String
            relOptoFn ">" = "gt"
            relOptoFn "==" = "eq"
            relOptoFn "and" = "and"
            relOptoFn "or" = "or"
            relOptoFn "&&" = "and"
            relOptoFn "||" = "or"

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
                                        content = Elem "content" [] [CElem kNode (),
                                                                     CElem (Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left lArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left rArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                        edge = mkEdge cId
                                     in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "fun"])]
                                                    [CElem content (), CElem edge ()]

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
                                                             [CElem valNode ()]
                                     conNode = contentNode colNode
                                     edgeNode = mkEdge cxId
                                  in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "attach"])] [CElem conNode (), CElem edgeNode ()]

-- Create an xml table node with one value in it
mkTableNode :: XMLNode -> ColName -> AVal -> ATy -> Element ()
mkTableNode xId n val ty = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                               colNode = Elem "column" [("name", AttValue [Left n])
                                                       ,("new", AttValue [Left "true"])]
                                                       [CElem valNode ()]
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
mkQueryPlan :: Maybe (Element ()) -> [Element ()] -> XML ()
mkQueryPlan props els = let logicalPlan = Elem "logical_query_plan" [("unique_names", AttValue [Left "true"])] $ map (\n -> CElem n ()) els
                            propPlan = case props of
                                            Nothing -> []
                                            Just e  -> [CElem e ()]
                         in do
                             planId <- freshId
                             tell $ [Elem "query_plan" [("id", AttValue [Left $ show planId])] $ propPlan ++ [CElem logicalPlan ()]]
                        
-- Create a plan bundle out of the given query plans
mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = Elem "query_plan_bundle" [] $ map (\p -> CElem p ()) plans

-- Create an xml document out of the given root tag.
mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prolog = Prolog (Just xmlDecl) [] Nothing []
                    in Document prolog emptyST el []

-- Create an xml property node so that ferryDB knows more or less how to print the result
mkProperties :: FType -> Element ()
mkProperties ty = let resTy = Elem "property" [("name", AttValue [Left "overallResultType"]), ("value", AttValue[Left result])] []
                   in Elem "properties" [] [CElem resTy ()]
    where
        result = case ty of
                    FList _ -> "LIST"
                    _       -> "TUPLE"