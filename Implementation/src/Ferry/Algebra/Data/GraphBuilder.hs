{-# LANGUAGE GADTs #-}
module Ferry.Algebra.Data.GraphBuilder where
    
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.Create

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

type GraphM = ReaderT (Gam, AlgNode) (State (Int, M.Map AlgNode Int))

type Gam = [(String, Int)]

data SubPlan where
    SubPlan :: String -> AlgRes -> SubPlan -> SubPlan
    EmptySub :: SubPlan

type AlgRes = (Int, Columns, SubPlan)

type AlgPlan = (M.Map AlgNode Int, AlgRes)

initLoop :: AlgNode
initLoop = litTable (nat 1) "iter" natT


runGraph :: GraphM AlgRes -> AlgPlan
runGraph = (\(r, (_,m)) -> (m, r) ) . flip runState (1, M.empty) . flip runReaderT ([], initLoop)

getLoop :: GraphM AlgNode
getLoop = do 
            (_, l) <- ask
            return l

getFreshId :: GraphM Int
getFreshId = do
                (n, t) <- get
                put $ (n + 1, t)
                return n

findNode :: AlgNode -> GraphM (Maybe Int)
findNode n = do
              (_, t) <- get
              return $ M.lookup n t
              
insertNode :: AlgNode-> GraphM Int
insertNode (n, children) = do
                            let ctx = (n, children)
                            v <- findNode ctx             
                            case v of
                                (Just n) -> return n
                                Nothing -> insertNode' n children
                                 
insertNode' :: Algebra -> [Int] -> GraphM Int
insertNode' n children = do 
                              i <- getFreshId 
                              (sup, t) <- get
                              let t' = M.insert (n, children) i t
                              put $ (sup, t')
                              return i

withBinding :: String -> Int -> GraphM a -> GraphM a
withBinding n v a = do
                     local (\(g, alg) -> ((n, v):g, alg)) a