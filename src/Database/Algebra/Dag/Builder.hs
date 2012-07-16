{-# LANGUAGE GADTs #-}
module Database.Algebra.Dag.Builder where
    
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

import Database.Algebra.Aux
import Database.Algebra.Dag.Common


data BuildState alg = BuildState {
  supply :: Int,
  algMap :: AlgMap alg,
  tags :: NodeMap [Tag] }

-- | Graphs are constructed in a monadic environment.
-- | The graph constructed has to be a DAG.
-- | The reader monad provides access to the variable environment Gamma and the loop table
-- | The variable environment is a mapping from variable names to graphnodes that represent
-- | their compiled form.
-- | The state monad gives access to a supply of fresh variables, and maintains a map from
-- | nodes to node ids. When a node is inserted and an equal node (equal means, equal node 
-- | and equal child nodes) already exists in the map the node id for that already existing
-- | node is returned. This allows maximal sharing.
type GraphM res alg = ReaderT (Gam res, AlgNode) (State (BuildState alg))

-- | Variable environemtn mapping from variables to compiled nodes.
type Gam a = [(String, a)]

-- | An algebraic plan is the result of constructing a graph.
-- | The pair consists of the mapping from nodes to their respective ids
-- | and the algres from the top node.
type AlgPlan alg res = (AlgMap alg, res, NodeMap [Tag])

-- | Evaluate the monadic graph into an algebraic plan, given a loop relation.

runGraph :: alg -> GraphM r alg res -> AlgPlan alg res
runGraph l =  constructAlgPlan . flip runState initialBuildState . flip runReaderT ([], 1)
  where initialBuildState = BuildState { supply = 2, algMap = M.singleton l 1, tags = M.empty }
        constructAlgPlan (r, s) = (algMap s, r, tags s)

reverseAlgMap :: AlgMap alg -> M.Map AlgNode alg
reverseAlgMap = reverseMap
                  

-- | Tag a subtree with a comment
tag :: String -> AlgNode -> GraphM res alg AlgNode
tag s c = do
  addTag c s
  return c

-- | Tag a subtree with a comment (monadic version)
tagM :: String -> GraphM res alg AlgNode -> GraphM res alg AlgNode
tagM s = (=<<) (tag s)

-- Add tag 
addTag :: AlgNode -> String -> GraphM res alg ()
addTag i c = modify insertTag 
  where
    -- insertTag :: (Int, M.Map Algebra AlgNode, NodeMap [Tag]) -> (Int, M.Map Algebra AlgNode, NodeMap [Tag])
    insertTag :: BuildState a -> BuildState a
    insertTag s = s { tags = M.insertWith (++) i [c] $ tags s }

-- | Get the current loop table
getLoop :: GraphM res alg AlgNode
getLoop = do 
            (_, l) <- ask
            return l

-- | Get the current variable environment            
getGamma :: GraphM res alg (Gam res)
getGamma = do
            (g, _) <- ask
            return g

-- | Get a fresh node id
getFreshId :: GraphM a res Int
getFreshId = do
                s <- get
                let n = supply s
                put $ s { supply = n + 1 }
                return n

-- | Check if a node already exists in the graph construction environment, if so return its id.
findNode :: Ord alg => alg -> GraphM res alg (Maybe AlgNode)
findNode n = do
              m <- gets algMap
              return $ M.lookup n m

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.              
insertNode :: Ord alg => alg -> GraphM res alg AlgNode
insertNode n = do
                            v <- findNode n             
                            case v of
                                (Just n') -> return n'
                                Nothing -> insertNode' n

-- | Blindly insert a node, get a fresh id and return that                                 
insertNode' :: Ord alg => alg  -> GraphM res alg AlgNode
insertNode' n = do 
                              i <- getFreshId 
                              s <- get
                              let m' = M.insert n i (algMap s)
                              put $ s { algMap = m' }
                              return i

-- | Evaluate the graph construction computation with the current environment extended with a binding n to v.
withBinding :: String -> a -> GraphM a alg r -> GraphM a alg r
withBinding n v a = do
                     local (\(g, alg) -> ((n, v):g, alg)) a

-- | Evaluate the graph construction computation with a differnt gamma, 
-- | and loop table. Return within he current computational context.                     
withContext :: Gam a -> AlgNode -> GraphM a alg r -> GraphM a alg r
withContext gam loop = local (\_ -> (gam, loop))

-- | Lookup a variable in the environment                     
fromGam :: String -> GraphM a alg a
fromGam n = do
             (m, _) <- ask
             case lookup n m of
                 Just r -> return r
                 Nothing -> error $ "Variable: " ++ n ++ " could not be found, should not be possible!"
                 
