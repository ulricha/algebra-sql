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
                                           plan = runXML nodeTable $ alg2XML top
                                           qPlan = runXML M.empty $ mkQueryPlan plan
                                           planBundle = mkPlanBundle qPlan
                                        in document $ mkXMLDocument planBundle

-- type AlgNode = (Algebra, [Int])
                                   
alg2XML :: GraphNode -> XML XMLNode 
alg2XML gId = do
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
    algrXML' (Attach (n, (ty, val)),[cId1]) = do
                                                xId <- freshId
                                                cxId1 <- alg2XML cId1
                                                tell [mkAttachNode xId n val ty cxId1]
                                                return xId



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

mkQueryPlan :: [Element ()] -> XML (Element ())
mkQueryPlan els = let logicalPlan = Elem "logical_query_plan" [("unique_names", AttValue [Left "true"])] $ map (\n -> CElem n ()) els
                   in do
                        planId <- freshId
                        return $ Elem "query_plan" [("id", AttValue [Left $ show planId])] [CElem logicalPlan ()]
                        

mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = Elem "query_plan_bundle" [] $ map (\p -> CElem p ()) plans

mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prolog = Prolog (Just xmlDecl) [] Nothing []
                    in Document prolog emptyST el []
{-
data Algebra where
    Proj       :: SemInfProj -> Algebra       -- should have one child   
    EqJoin     :: SemInfEqJoin -> Algebra     -- should have two children 
    LitTable   :: SemInfLitTable -> SchemaInfos -> Algebra
    Attach     :: SemInfAttach -> Algebra     -- should have one child
    FunBinOp   :: SemBinOp -> Algebra         -- should have one child

-}

