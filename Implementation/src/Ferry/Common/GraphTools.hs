module Ferry.Common.GraphTools where
    
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Tree

import qualified Data.Map as M
import Control.Monad.State

type GraphM a b = State (Int, Gr a b, M.Map (a, [Int]) Int) 

runGraph :: GraphM a b c -> Gr a b
runGraph = (\(_,n,_) -> n) . snd . flip runState (1, empty, M.empty)

getFreshId :: GraphM a b Int
getFreshId = do
                (n, s, t) <- get
                put $ (n + 1, s, t)
                return n

findNode :: Ord a => (a, [Int]) -> GraphM a b (Maybe Int)
findNode n = do
              (_, _, t) <- get
              return $ M.lookup n t
              
insertNode :: Ord a => a -> [Int] -> b -> GraphM a b Int
insertNode n children l = do
                        let ctx = (n, children)
                        v <- findNode ctx             
                        case v of
                            (Just n) -> return n
                            Nothing -> insertNode' n children l
                                 
insertNode' :: Ord a => a -> [Int] -> b -> GraphM a b Int
insertNode' n children lab = do 
                              i <- getFreshId 
                              (sup, s, t) <- get
                              let s' = ([], i, n, [(lab, c) | c <- children]) & s
                              let t' = M.insert (n, children) i t
                              put $ (sup, s', t')
                              return i
                  
insertEdge :: Int -> Int -> b ->  GraphM a b ()
insertEdge from to lab = do
                            (sup, s, t) <- get
                            let s' = insEdge (from, to, lab) s
                            put (sup, s', t)
                            
insertEdges :: Int -> [Int] -> b -> GraphM a b ()
insertEdges from (x:xs) lab = do
                                insertEdge from x lab
                                insertEdges from xs lab
insertEdges _ [] _          = return ()
