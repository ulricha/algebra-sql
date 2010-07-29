{-# LANGUAGE GADTs #-}
module Ferry.Algebra.Data.GraphBuilder where
    
import Ferry.Algebra.Data.Algebra
--import Ferry.Algebra.Data.Create

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

type GraphM = ReaderT (Gam, AlgNode) (State (Int, M.Map AlgConstr AlgNode))

type Gam = [(String, AlgRes)]

data SubPlan where
    SubPlan :: String -> AlgRes -> SubPlan -> SubPlan
    EmptySub :: SubPlan

type AlgRes = (AlgNode, Columns, SubPlan)

type AlgPlan = (M.Map AlgConstr AlgNode, AlgRes)

runGraph :: AlgConstr -> GraphM AlgRes -> AlgPlan
runGraph l = (\(r, (_,m)) -> (m, r) ) . flip runState (2, M.singleton l 1) . flip runReaderT ([], 1)

getLoop :: GraphM AlgNode
getLoop = do 
            (_, l) <- ask
            return l
            
getGamma :: GraphM Gam
getGamma = do
            (g, _) <- ask
            return g

getFreshId :: GraphM Int
getFreshId = do
                (n, t) <- get
                put $ (n + 1, t)
                return n

findNode :: AlgConstr -> GraphM (Maybe AlgNode)
findNode n = do
              (_, t) <- get
              return $ M.lookup n t
              
insertNode :: AlgConstr-> GraphM AlgNode
insertNode (n, children) = do
                            let ctx = (n, children)
                            v <- findNode ctx             
                            case v of
                                (Just n) -> return n
                                Nothing -> insertNode' n children
                                 
insertNode' :: Algebra -> [AlgNode] -> GraphM AlgNode
insertNode' n children = do 
                              i <- getFreshId 
                              (sup, t) <- get
                              let t' = M.insert (n, children) i t
                              put $ (sup, t')
                              return i

withBinding :: String -> AlgRes -> GraphM a -> GraphM a
withBinding n v a = do
                     local (\(g, alg) -> ((n, v):g, alg)) a
                     
withContext :: Gam -> AlgNode -> GraphM a -> GraphM a
withContext gam loop = local (\_ -> (gam, loop))
                     
fromGam :: String -> GraphM AlgRes
fromGam n = do
             (m, _) <- ask
             case lookup n m of
                 Just r -> return r
                 Nothing -> error $ "Variable: " ++ n ++ " could not be found, should not be possible!"
                 
