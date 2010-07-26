module Ferry.Algebra.Render.XML where
    
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad


import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M

type ColName = String

type Graph = (Int, [(AlgNode, Int)])
type GraphNode = Int
type XMLNode = Int
type Dictionary = M.Map GraphNode XMLNode

type XML = WriterT [Element ()] (ReaderT (M.Map Int AlgNode) (State (Int, Dictionary)))

{-
type AlgRes = (Int, Columns, SubPlan)

type AlgPlan = (M.Map AlgNode Int, AlgRes)
-}

isDefined :: GraphNode -> XML (Maybe XMLNode)
isDefined g = do
                (_, d) <- get
                return $ M.lookup g d 

freshId :: XML Int
freshId = do
            (n, d) <- get
            put (n + 1, d)
            return n
            
addNodeTrans :: GraphNode -> XMLNode -> XML ()
addNodeTrans gId xId = do
                        (n, d) <- get
                        put (n, M.insert gId xId d)

getNode :: Int -> XML AlgNode
getNode i = do
             nodes <- ask
             return $ nodes M.! i


runXML :: M.Map Int AlgNode -> XML a -> [Element ()]
runXML m = snd . fst . flip runState (0, M.empty) . flip runReaderT m . runWriterT 

transform :: AlgPlan -> Doc
transform (nodes, (top, cols, subs)) = let nodeTable = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList nodes
                                           plan = runXML nodeTable $ serializeAlgebra top cols
                                           qPlan = runXML M.empty $ mkQueryPlan plan
                                           planBundle = mkPlanBundle qPlan
                                        in (document $ mkXMLDocument planBundle)


serializeAlgebra :: GraphNode -> Columns -> XML XMLNode
serializeAlgebra qGId cols = do
                                    qId <- alg2XML qGId
                                    nilId <- nilNode
                                    xId <- freshId
                                    let contentN = Elem "content" [] $ (:) (CElem iterCol ()) $ (:) (CElem posCol ()) $ map (\c -> CElem c ()) $ colsToNodes $ zip cols [1..]
                                    let edgeNil = mkEdge nilId
                                    let edgeQ = mkEdge qId
                                    tell [Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "serialize relation"])] [CElem contentN (), CElem edgeNil (), CElem edgeQ ()]]
                                    return xId

iterCol :: Element ()
iterCol = Elem "column" [("name", AttValue [Left "iter"]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "iter"])] []

posCol :: Element ()
posCol = Elem "column" [("name", AttValue [Left "pos"]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "pos"])] []
                                    
colsToNodes :: [(Column, Int)] -> [Element ()]
colsToNodes ((Col name, nr):cols) = let col = Elem "column" [("name", AttValue [Left name]), ("new", AttValue [Left "false"]), ("function", AttValue [Left "item"]), ("position", AttValue [Left $ show nr])] []
                                     in (:) col $ colsToNodes cols
colsToNodes []                    = []

nilNode :: XML XMLNode
nilNode = do
            xId <- freshId
            tell [Elem "node" [("id", AttValue [Left $ show xId]),("kind", AttValue [Left "nil"])] []]
            return xId
            

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
    alg2XML' :: AlgNode -> XML XMLNode 
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
                                                            
mkBinOpNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkBinOpNode xId op res lArg rArg cId | elem op ["+", "-", "*", "%", "/"] = mkFnNode xId (arOptoFn op) res lArg rArg cId
                                     | elem op [">", "==", "and", "or", "&&", "||"] = mkRelFnNode xId (relOptoFn op) res lArg rArg cId
                                     | elem op ["<" ] = mkBinOpNode xId ">" res rArg lArg cId
        where
            arOptoFn :: String -> String
            arOptoFn "+" = "add"
            arOptoFn "-" = "substract"
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

mkRelFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkRelFnNode xId fn res lArg rArg cId = let content = Elem "content" [] [CElem (Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []) (),
                                                                        CElem (Elem "column" [("name", AttValue [Left lArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) (),
                                                                        CElem (Elem "column" [("name", AttValue [Left rArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                           edge = mkEdge cId
                                        in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left fn])]
                                                       [CElem content (), CElem edge ()]
            
mkFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkFnNode xId fn res lArg rArg cId = let kNode = Elem "kind" [("name", AttValue [Left fn])] []
                                        content = Elem "content" [] [CElem kNode (),
                                                                     CElem (Elem "column" [("name", AttValue [Left res]), ("new", AttValue [Left "true"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left lArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) (),
                                                                     CElem (Elem "column" [("name", AttValue [Left rArg]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                        edge = mkEdge cId
                                     in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "fun"])]
                                                    [CElem content (), CElem edge ()]
            
mkEqJoinNode :: XMLNode -> (LeftAttrName,RightAttrName) -> XMLNode -> XMLNode -> Element ()
mkEqJoinNode xId (lN, rN) cxId1 cxId2 = let contNode = Elem "content" [] [CElem (Elem "column" [("name", AttValue [Left lN]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "1"])] []) ()
                                                                         ,CElem (Elem "column" [("name", AttValue [Left rN]), ("new", AttValue [Left "false"]), ("position", AttValue [Left "2"])] []) ()]
                                            edge1 = mkEdge cxId1
                                            edge2 = mkEdge cxId2
                                         in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "eqjoin"])]
                                                        [CElem contNode (), CElem edge1 (), CElem edge2 ()]
                                                        

mkProjNode :: XMLNode -> [(NewAttrName, OldAttrName)] -> XMLNode -> Element ()
mkProjNode xId mapping cxId = let contNode = Elem "content" [] $ map (\m -> flip CElem () $ mkProjColumn m) mapping
                                  edgeNode = mkEdge cxId
                               in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "project"])]
                                              [CElem contNode (), CElem edgeNode ()]
    where
      mkProjColumn :: (NewAttrName, OldAttrName) -> Element ()
      mkProjColumn (n, o) = Elem "column" [("name", AttValue [Left n]), ("old_name", AttValue [Left o]), ("new", AttValue [Left "true"])] []

mkAttachNode :: XMLNode -> ColName -> AVal -> ATy -> XMLNode -> Element ()
mkAttachNode xId n val ty cxId = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                                     colNode = Elem "column" [("name", AttValue [Left n])
                                                             ,("new", AttValue [Left "true"])]
                                                             [CElem valNode ()]
                                     conNode = contentNode colNode
                                     edgeNode = mkEdge cxId
                                  in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "attach"])] [CElem conNode (), CElem edgeNode ()]

mkTableNode :: XMLNode -> ColName -> AVal -> ATy -> Element ()
mkTableNode xId n val ty = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                               colNode = Elem "column" [("name", AttValue [Left n])
                                                       ,("new", AttValue [Left "true"])]
                                                       [CElem valNode ()]
                               conNode = contentNode colNode
                            in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "table"])] [CElem conNode ()] 

mkEdge :: XMLNode -> Element ()
mkEdge n = Elem "edge" [("to", AttValue [Left $ show n])] []

contentNode :: Element () -> Element ()
contentNode n = Elem "content" [] [CElem n ()]

mkQueryPlan :: [Element ()] -> XML ()
mkQueryPlan els = let logicalPlan = Elem "logical_query_plan" [("unique_names", AttValue [Left "true"])] $ map (\n -> CElem n ()) els
                   in do
                        planId <- freshId
                        tell $ [Elem "query_plan" [("id", AttValue [Left $ show planId])] [CElem logicalPlan ()]]
                        

mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = Elem "query_plan_bundle" [] $ map (\p -> CElem p ()) plans

mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prolog = Prolog (Just xmlDecl) [] Nothing []
                    in Document prolog emptyST el []


