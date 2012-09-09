{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

-- | This module provides a monadic interface to rewrites on algebra DAGs.
module Database.Algebra.Rewrite.DagRewrite 
       (
         -- ** The Rewrite monad
         DagRewrite(..)
       , DefaultRewrite(..)
       , runDefaultRewrite
       , initRewriteState
       , Log
       ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
  
import Database.Algebra.Dag.Common
import qualified Database.Algebra.Dag as Dag

class Monad r => (DagRewrite r) o | r -> o where
  -- | Log a general message
  logGeneral :: String -> r ()
  -- | Log a rewrite
  logRewrite :: String -> AlgNode -> r ()
  -- | Return the set of nodes that are reachable from the specified node.
  reachableNodesFrom :: AlgNode -> r (S.Set AlgNode)
  -- | Return the parents of a node
  parents :: AlgNode -> r [AlgNode]
  -- | Return a topological ordering of all reachable nodes in the DAG. 
  topsort :: Dag.Operator o => r [AlgNode]
  -- | Return the operator for a node id.
  operator :: AlgNode -> r o
  -- | Returns the root nodes of the DAG.
  rootNodes :: r [AlgNode]
  -- | Exposes the current state of the DAG
  getDag :: r (Dag.AlgebraDag o)
  -- | Insert an operator into the DAG and return its node id.
  insert :: Dag.Operator o => o -> r AlgNode
  -- | replaceChildM n old new replaces all links from node n to node old with links
  --   to node new 
  replaceChild :: Dag.Operator o => AlgNode -> AlgNode -> AlgNode -> r ()
  -- | relinkParents old new replaces _all_ links to old with links to new
  relinkParents :: Dag.Operator o => AlgNode -> AlgNode -> r ()
  -- | Creates a new node from the operator and replaces the old node with it
  -- by rewireing all links to the old node.
  relinkToNew :: Dag.Operator o => AlgNode -> o -> r AlgNode
  -- | Replaces the operator at the specified node id with a new operator.
  replace :: Dag.Operator o => AlgNode -> o -> r ()
  -- | Remove all unreferenced nodes from the DAG: all nodes are unreferenced which
  -- are not reachable from one of the root nodes.
  pruneUnused :: r ()
  -- | Replaces an entry in the list of root nodes.
  replaceRoot :: AlgNode -> AlgNode -> r ()
  -- | Apply a pure function to the DAG.
  infer :: (Dag.AlgebraDag o -> b) -> r b
  
-- | Cache some topological information about the DAG.
data Cache = Cache { cachedTopOrdering :: Maybe [AlgNode] } 
             
emptyCache :: Cache
emptyCache = Cache Nothing 

data RewriteState o = RewriteState { nodeIDSupply   :: AlgNode       -- ^ Supply of fresh node ids
                                   , dag            :: Dag.AlgebraDag o  -- ^ The DAG itself
                                   , cache          :: Cache         -- ^ Cache of some topological information
                                   }
                      
-- | A Monad for DAG rewrites, parameterized over the type of algebra operators.
newtype DefaultRewrite o a = D (WriterT Log (State (RewriteState o)) a) deriving (Monad, Functor, Applicative)
                                                                             
-- FIXME Map.findMax might call error
initRewriteState :: Dag.AlgebraDag a -> RewriteState a
initRewriteState d =
    let maxID = fst $ M.findMax $ Dag.nodeMap d
    in RewriteState { nodeIDSupply = maxID + 1, dag = d, cache = emptyCache }
                                                               
-- | Run a rewrite action on the supplied graph. Returns the rewritten node map, the potentially
-- modified list of root nodes, the result of the rewrite and the rewrite log.
runDefaultRewrite :: Dag.Operator a => DefaultRewrite a r -> Dag.AlgebraDag a -> (Dag.AlgebraDag a, r, Log)
runDefaultRewrite (D m) d = (dag s, res, rewriteLog) 
  where ((res, rewriteLog), s) = runState (runWriterT m) (initRewriteState d)  
        
-- | The log from a sequence of rewrite actions.
type Log = Seq.Seq String

-- | Return a fresh node id (only used internally).
freshNodeID :: DefaultRewrite a AlgNode
freshNodeID =
  D $ do
    s <- get
    let n = nodeIDSupply s
    put $ s { nodeIDSupply = n + 1 }
    return n
  
-- FIXME unwrapD should not be necessary: just provide a type alias for the monad stack
unwrapD :: DefaultRewrite a b -> WriterT Log (State (RewriteState a)) b
unwrapD (D m) = m
                
invalidateCacheM :: DefaultRewrite o ()
invalidateCacheM =
  D $ do
    s <- get
    put $ s { cache = emptyCache }
  
putDag :: Dag.AlgebraDag o -> DefaultRewrite o ()
putDag d =
  D $ do
    s <- get
    put $ s { dag = d }
  
putCache :: Cache -> DefaultRewrite o ()
putCache c =
  D $ do
    s <- get
    put $ s { cache = c }
           
instance DagRewrite (DefaultRewrite o) o where
  logGeneral s = D $ tell $ Seq.singleton s

  logRewrite rewrite node = 
    logGeneral $ "Triggering rewrite " ++ rewrite ++ " at node " ++ (show node)
           
  reachableNodesFrom n =
    D $ do
      d <- gets dag
      return $ Dag.reachableNodesFrom n d
  
  parents n = 
    D $ do
      d <- gets dag
      return $ Dag.parents n d

  topsort = 
    D $ do
      s <- get
      let c = cache s
      case cachedTopOrdering c of
        Just o -> return o
        Nothing -> do
          let d = dag s
              ordering = Dag.topsort d
          unwrapD $ putCache $ c { cachedTopOrdering = Just ordering }
          return ordering
 
  operator n = 
    D $ do
      d <- gets dag
      return $ Dag.operator n d
  
  rootNodes = D $ liftM Dag.rootNodes $ liftM dag $ get
  
  getDag = D $ gets dag
  
  insert op = 
    D $ do
      n <- unwrapD freshNodeID
      s <- get
      unwrapD invalidateCacheM
      unwrapD $ putDag $ Dag.insert n op $ dag s
      return n
  
  replaceChild n old new = 
    D $ do
      s <- get
      unwrapD invalidateCacheM
      unwrapD $ putDag $ Dag.replaceChild n old new $ dag s
   
  relinkParents old new = do
    ps <- parents old
    forM_ ps $ (\p -> replaceChild p old new)
  
  relinkToNew oldNode newOp = do
    newNode <- insert newOp
    rs <- rootNodes
    relinkParents oldNode newNode
    if oldNode `elem` rs
      then replaceRoot oldNode newNode
      else return ()
    return newNode
  
  replace node newOp =
    D $ do
      d <- gets dag
      unwrapD invalidateCacheM
      unwrapD $ putDag $ Dag.replace node newOp d
  
  pruneUnused =
    D $ do
      s <- get
      case Dag.pruneUnused $ dag s of
        Just dag' -> do
          unwrapD invalidateCacheM
          unwrapD $ putDag dag'
        Nothing -> return ()
    
  replaceRoot oldRoot newRoot = do
    D $ do 
      s <- get
      if not $ M.member newRoot $ Dag.nodeMap $ dag s
        then error "replaceRootM: new root node is not present in the DAG"
        else unwrapD $ putDag $ Dag.replaceRoot (dag s) oldRoot newRoot
  
  infer f = D $ liftM f $ gets dag
