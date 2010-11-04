{-# LANGUAGE GADTs #-}
module Ferry.Algebra.Data.GraphBuilder where
    
import Ferry.Algebra.Data.Algebra
--import Ferry.Algebra.Data.Create

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

-- | Graphs are constructed in a monadic environment.
-- | The graph constructed has to be a DAG.
-- | The reader monad provides access to the variable environment Gamma and the loop table
-- | The variable environment is a mapping from variable names to graphnodes that represent
-- | their compiled form.
-- | The state monad gives access to a supply of fresh variables, and maintains a map from
-- | nodes to node ids. When a node is inserted and an equal node (equal means, equal node 
-- | and equal child nodes) already exists in the map the node id for that already existing
-- | node is returned. This allows maximal sharing.
type GraphM = ReaderT (Gam, AlgNode) (State (Int, M.Map Algebra AlgNode))

-- | Variable environemtn mapping from variables to compiled nodes.
type Gam = [(String, AlgRes)]

newtype SubPlan = SubPlan (M.Map Int AlgRes)

instance Show SubPlan where
    show (SubPlan p) = "SubPlans " ++ (show $ map (\(_,y,z) -> show (y, z)) $ M.elems p)
    
emptyPlan :: SubPlan
emptyPlan = SubPlan M.empty

subPlan :: Int -> AlgRes -> SubPlan
subPlan i p = SubPlan $ M.singleton i p

getPlan :: Int -> SubPlan -> AlgRes
getPlan i (SubPlan p) = p M.! i
-- | An algebraic solution is a triple consisting of the node id, a description of the database columns and all subplans
type AlgRes = (AlgNode, Columns, SubPlan)

-- | An algebraic plan is the result of constructing a graph.
-- | The pair consists of the mapping from nodes to their respective ids
-- | and the algres from the top node.
type AlgPlan = (M.Map Algebra AlgNode, AlgRes)

-- | Evaluate the monadic graph into an algebraic plan, given a loop relation.
runGraph :: Algebra -> GraphM AlgRes -> AlgPlan
runGraph l = (\(r, (_,m)) -> (m, r) ) . flip runState (2, M.singleton l 1) . flip runReaderT ([], 1)

-- | Get the current loop table
getLoop :: GraphM AlgNode
getLoop = do 
            (_, l) <- ask
            return l

-- | Get the current variable environment            
getGamma :: GraphM Gam
getGamma = do
            (g, _) <- ask
            return g

-- | Get a fresh node id
getFreshId :: GraphM Int
getFreshId = do
                (n, t) <- get
                put $ (n + 1, t)
                return n

-- | Check if a node already exists in the graph construction environment, if so return its id.
findNode :: Algebra -> GraphM (Maybe AlgNode)
findNode n = do
              (_, t) <- get
              return $ M.lookup n t

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.              
insertNode :: Algebra -> GraphM AlgNode
insertNode n = do
                            v <- findNode n             
                            case v of
                                (Just n') -> return n'
                                Nothing -> insertNode' n

-- | Blindly insert a node, get a fresh id and return that                                 
insertNode' :: Algebra  -> GraphM AlgNode
insertNode' n = do 
                              i <- getFreshId 
                              (sup, t) <- get
                              let t' = M.insert n i t
                              put $ (sup, t')
                              return i

-- | Evaluate the graph construction computation with the current environment extended with a binding n to v.
withBinding :: String -> AlgRes -> GraphM a -> GraphM a
withBinding n v a = do
                     local (\(g, alg) -> ((n, v):g, alg)) a

-- | Evaluate the graph construction computation with a differnt gamma, 
-- | and loop table. Return within he current computational context.                     
withContext :: Gam -> AlgNode -> GraphM a -> GraphM a
withContext gam loop = local (\_ -> (gam, loop))

-- | Lookup a variable in the environment                     
fromGam :: String -> GraphM AlgRes
fromGam n = do
             (m, _) <- ask
             case lookup n m of
                 Just r -> return r
                 Nothing -> error $ "Variable: " ++ n ++ " could not be found, should not be possible!"
                 
