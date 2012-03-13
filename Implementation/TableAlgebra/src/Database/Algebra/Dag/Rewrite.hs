{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides a monadic interface to rewrites on algebra DAGs.
module Database.Algebra.Dag.Rewrite 
       (
         -- * The Rewrite monad
         DagRewrite
       , runRewrite
       , initRewriteState
         -- * Rewrite logging
       , Log
       , logGeneralM
       , logRewriteM
         -- * Query for topological information
       , hasPathM
       , parentsM
       , topsortM
       , reachableNodesFromM
       , inferM
         -- * Query for operator information
       , operatorM
       , rootNodesM
         -- * DAG modification
       , insertM
       , replaceChildM
       , relinkParentsM
       , relinkToNewM
       , replaceM
       , replaceRootM
         -- * House cleaning
       , pruneUnusedM
       ) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
  
import Database.Algebra.Graph.Common
import Database.Algebra.Dag
  
-- | Cache some topological information about the DAG.
data Cache = Cache { cachedTopOrdering      :: Maybe [AlgNode]
                   , cachedReachableNodes   :: Maybe (S.Set AlgNode)
                   } 
             
emptyCache :: Cache
emptyCache = Cache Nothing Nothing
                

data RewriteState a = RewriteState { nodeIDSupply   :: AlgNode       -- ^ Supply of fresh node ids
                                   , dag            :: AlgebraDag a  -- ^ The DAG itself
                                   , cache          :: Cache         -- ^ Cache of some topological information
                                   }
                      
                      
-- | A Monad for DAG rewrites, parameterized over the type of algebra operators.
newtype DagRewrite a r = D (WriterT Log (State (RewriteState a)) r) deriving (Monad)
                                                                             
-- FIXME Map.findMax might call error
initRewriteState :: AlgebraDag a -> RewriteState a
initRewriteState d =
    let maxID = fst $ M.findMax $ nodeMap d
    in RewriteState { nodeIDSupply = maxID + 1, dag = d, cache = Cache Nothing Nothing }
                                                               
-- | Run a rewrite action on the supplied graph. Returns the rewritten node map, the potentially
-- modified list of root nodes, the result of the rewrite and the rewrite log.
runRewrite :: Operator a => DagRewrite a r -> AlgebraDag a -> (AlgebraDag a, r, Log)
runRewrite (D m) d = (dag s, res, rewriteLog) 
  where ((res, rewriteLog), s) = runState (runWriterT m) (initRewriteState d)  
        
-- | The log from a sequence of rewrite actions.
type Log = Seq.Seq String
           
-- | Log a general message
logGeneralM :: String -> DagRewrite a ()
logGeneralM s = D $ tell $ Seq.singleton s

-- | Log a rewrite
logRewriteM :: Show s => String -> AlgNode -> s -> DagRewrite a ()
logRewriteM rewrite node op = 
  logGeneralM $ "Triggering rewrite " ++ rewrite ++ " at node " ++ (show node) ++ " with operator " ++ (show op)
           
  
-- | hasPath a b returns 'True' iff there is a path from a to b in the DAG.
hasPathM :: AlgNode -> AlgNode -> DagRewrite a Bool
hasPathM a b =
  D $ do
    d <- gets dag
    return $ hasPath a b d
  
-- | Return the set of nodes that are reachable from the specified node.
reachableNodesFromM :: AlgNode -> DagRewrite a (S.Set AlgNode)
reachableNodesFromM n =
  D $ do
    d <- gets dag
    return $ reachableNodesFrom n d
  
-- | Return the parents of a node
parentsM :: AlgNode -> DagRewrite a [AlgNode]
parentsM n = 
  D $ do
    d <- gets dag
    return $ parents n d

-- | Return a topological ordering of all reachable nodes in the DAG. 
topsortM :: Operator a => DagRewrite a [AlgNode]
topsortM = 
  D $ do
    s <- get
    let c = cache s
    case cachedTopOrdering c of
      Just o -> return o
      Nothing -> do
        let d = dag s
            ordering = topsort d
        put $ s { cache = c { cachedTopOrdering = Just ordering } }
        return ordering
 
-- | Return the operator for a node id.
operatorM :: AlgNode -> DagRewrite a a
operatorM n = 
  D $ do
    d <- gets dag
    return $ operator n d
  
-- | Returns the root nodes of the DAG.
rootNodesM :: DagRewrite a [AlgNode]
rootNodesM = D $ liftM rootNodes $ liftM dag $ get
  
-- | Return a fresh node id (only used internally).
freshNodeID :: DagRewrite a AlgNode
freshNodeID =
  D $ do
    s <- get
    let n = nodeIDSupply s
    put $ s { nodeIDSupply = n + 1 }
    return n
  
-- FIXME unwrapD should not be necessary: just provide a type alias for the monad stack
unwrapD :: DagRewrite a b -> WriterT Log (State (RewriteState a)) b
unwrapD (D m) = m
                
invalidateCacheM :: DagRewrite a ()
invalidateCacheM =
  D $ do
    s <- get
    put $ s { cache = emptyCache }
  
putDag :: AlgebraDag a -> DagRewrite a ()
putDag d =
  D $ do
    s <- get
    put $ s { dag = d }
  
-- | Insert an operator into the DAG and return its node id.
insertM :: Operator a => a -> DagRewrite a AlgNode
insertM op = 
  D $ do
    n <- unwrapD freshNodeID
    s <- get
    unwrapD invalidateCacheM
    unwrapD $ putDag $ insert n op $ dag s
    return n
  
-- | replaceChildM n old new replaces all links from node n to node old with links
--   to node new 
replaceChildM :: Operator a => AlgNode -> AlgNode -> AlgNode -> DagRewrite a ()
replaceChildM n old new = 
   D $ do
     s <- get
     unwrapD invalidateCacheM
     unwrapD $ putDag $ replaceChild n old new $ dag s
   
-- | relinkParents old new replaces _all_ links to old with links to new
relinkParentsM :: Operator a => AlgNode -> AlgNode -> DagRewrite a ()
relinkParentsM old new = do
  ps <- parentsM old
  forM_ ps $ (\p -> replaceChildM p old new)
  
-- | Creates a new node from the operator and replaces the old node with it
-- by rewireing all links to the old node.
relinkToNewM :: Operator a => AlgNode -> a -> DagRewrite a ()
relinkToNewM oldNode newOp = do
  newNode <- insertM newOp
  relinkParentsM oldNode newNode
  
-- | Replaces the operator at the specified node id with a new operator.
replaceM :: Operator a => AlgNode -> a -> DagRewrite a ()
replaceM node newOp =
  D $ do
    d <- gets dag
    unwrapD invalidateCacheM
    unwrapD $ putDag $ replace node newOp d
  
-- | Remove all unreferenced nodes from the DAG: all nodes are unreferenced which
-- are not reachable from one of the root nodes.
pruneUnusedM :: DagRewrite a ()
pruneUnusedM =
  D $ do
    s <- get
    case pruneUnused $ dag s of
      Just dag' -> do
        unwrapD invalidateCacheM
        unwrapD $ putDag dag'
      Nothing -> return ()
    
-- | Replaces an entry in the list of root nodes.
replaceRootM :: AlgNode -> AlgNode -> DagRewrite a ()
replaceRootM oldRoot newRoot = do
  D $ do 
    s <- get
    if M.member newRoot $ nodeMap $ dag s
      then error "replaceRootM: new root node is not present in the DAG"
      else unwrapD $ putDag $ replaceRoot (dag s) oldRoot newRoot
  
-- | Apply a pure function to the DAG.
inferM :: (AlgebraDag a -> b) -> DagRewrite a b
inferM f = D $ liftM f $ liftM dag $ get
