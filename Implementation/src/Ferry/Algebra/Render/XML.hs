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
                                        in undefined

transformPlan :: AlgRes -> XML ()                                            
transformPlan (top, cols, subs) = do
                                   alg2XML top
                                   return () 
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
                                            let node = mkTableNode xId n v ty
                                            tell [node]
                                            return xId 
                                          


mkTableNode :: XMLNode -> ColName -> AVal -> ATy -> Element ()
mkTableNode xId n val ty = let valNode = Elem "value" [("type", AttValue [Left $ show ty])] [CString False (show val) ()]
                               colNode = Elem "column" [("name", AttValue [Left n])
                                                       ,("new", AttValue [Left "true"])]
                                                       [CElem valNode ()]
                               conNode = Elem "content" [] [CElem colNode ()]
                            in Elem "node" [("id", AttValue [Left $ show xId]), ("kind", AttValue [Left "table"])] [CElem conNode ()] 
{-
data Algebra where
    Proj       :: SemInfProj -> Algebra       -- should have one child   
    EqJoin     :: SemInfEqJoin -> Algebra     -- should have two children 
    LitTable   :: SemInfLitTable -> SchemaInfos -> Algebra
    Attach     :: SemInfAttach -> Algebra     -- should have one child
    FunBinOp   :: SemBinOp -> Algebra         -- should have one child

-}

