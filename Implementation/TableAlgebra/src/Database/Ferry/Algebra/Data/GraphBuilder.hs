{-# LANGUAGE GADTs #-}
module Database.Ferry.Algebra.Data.GraphBuilder where
    
import Database.Ferry.Algebra.Data.Algebra

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
type GraphM a = ReaderT (Gam a, AlgNode) (State (Int, M.Map Algebra AlgNode, Tags))

-- | Variable environemtn mapping from variables to compiled nodes.
type Gam a = [(String, a)]


-- * Ferry specific 

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

-- End of ferry specific section

-- | An algebraic plan is the result of constructing a graph.
-- | The pair consists of the mapping from nodes to their respective ids
-- | and the algres from the top node.
type AlgPlan res = (M.Map Algebra AlgNode, res, Tags)

type Tags = M.Map AlgNode [String]

-- | Evaluate the monadic graph into an algebraic plan, given a loop relation.
runGraph :: Algebra -> GraphM res res -> AlgPlan res
runGraph l = (\(r, (_,m, c)) -> (m, r, c) ) . flip runState (2, M.singleton l 1, M.empty) . flip runReaderT ([], 1)

-- Add tag 
addTag :: AlgNode -> String -> GraphM a ()
addTag i c = modify insertTag 
  where
    insertTag :: (Int, M.Map Algebra AlgNode, Tags) -> (Int, M.Map Algebra AlgNode, Tags)
    insertTag (s, g, v) = (s, g, M.insertWith (++) i [c] v)

-- | Get the current loop table
getLoop :: GraphM a AlgNode
getLoop = do 
            (_, l) <- ask
            return l

-- | Get the current variable environment            
getGamma :: GraphM a (Gam a)
getGamma = do
            (g, _) <- ask
            return g

-- | Get a fresh node id
getFreshId :: GraphM a Int
getFreshId = do
                (n, t, c) <- get
                put $ (n + 1, t, c)
                return n

-- | Check if a node already exists in the graph construction environment, if so return its id.
findNode :: Algebra -> GraphM a (Maybe AlgNode)
findNode n = do
              (_, t, _) <- get
              return $ M.lookup n t

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.              
insertNode :: Algebra -> GraphM a AlgNode
insertNode (Dummy s c) = do
                            addTag c s
                            return c
insertNode n = do
                            v <- findNode n             
                            case v of
                                (Just n') -> return n'
                                Nothing -> insertNode' n

-- | Blindly insert a node, get a fresh id and return that                                 
insertNode' :: Algebra  -> GraphM a AlgNode
insertNode' n = do 
                              i <- getFreshId 
                              (sup, t, c) <- get
                              let t' = M.insert n i t
                              put $ (sup, t', c)
                              return i

-- | Evaluate the graph construction computation with the current environment extended with a binding n to v.
withBinding :: String -> a -> GraphM a r -> GraphM a r
withBinding n v a = do
                     local (\(g, alg) -> ((n, v):g, alg)) a

-- | Evaluate the graph construction computation with a differnt gamma, 
-- | and loop table. Return within he current computational context.                     
withContext :: Gam a -> AlgNode -> GraphM a r -> GraphM a r
withContext gam loop = local (\_ -> (gam, loop))

-- | Lookup a variable in the environment                     
fromGam :: String -> GraphM a a
fromGam n = do
             (m, _) <- ask
             case lookup n m of
                 Just r -> return r
                 Nothing -> error $ "Variable: " ++ n ++ " could not be found, should not be possible!"
                 
