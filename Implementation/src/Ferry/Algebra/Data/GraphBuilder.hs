module Ferry.Algebra.Data.GraphBuilder where
    
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Tree

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.Create

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

type Link = Adj ()

type ContextN = Context Algebra ()

type GraphM = ReaderT (Gam, Algebra) (State (Int, Gr Algebra (), M.Map AlgNode Int))

type Gam = [(String, AlgNode)]

initLoop :: Algebra
initLoop = undefined

runGraph :: GraphM c -> Gr Algebra ()
runGraph = (\(_,n,_) -> n) . snd . flip runState (1, empty, M.empty) . flip runReaderT ([], initLoop)

getFreshId :: GraphM Int
getFreshId = do
                (n, s, t) <- get
                put $ (n + 1, s, t)
                return n

findNode :: AlgNode -> GraphM (Maybe Int)
findNode n = do
              (_, _, t) <- get
              return $ M.lookup n t
              
insertNode :: Algebra -> [Int] -> GraphM Int
insertNode n children = do
                        let ctx = (n, children)
                        v <- findNode ctx             
                        case v of
                            (Just n) -> return n
                            Nothing -> insertNode' n children
                                 
insertNode' :: Algebra -> [Int] -> GraphM Int
insertNode' n children = do 
                              i <- getFreshId 
                              (sup, s, t) <- get
                              let s' = ([], i, n, [((), c) | c <- children]) & s
                              let t' = M.insert (n, children) i t
                              put $ (sup, s', t')
                              return i

insertEdge :: Int -> Int ->  GraphM ()
insertEdge from to = do
                      (sup, s, t) <- get
                      let s' = insEdge (from, to, ()) s
                      put (sup, s', t)

insertEdges :: Int -> [Int] -> GraphM ()
insertEdges from (x:xs)  = do
                                insertEdge from x
                                insertEdges from xs
insertEdges _ []           = return ()
                
