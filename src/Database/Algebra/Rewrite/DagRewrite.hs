{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides a monadic interface to rewrites on algebra DAGs.
module Database.Algebra.Rewrite.DagRewrite 
       (
         -- ** The Rewrite monad
         Rewrite
       , runRewrite
       , initRewriteState
       , Log
       , logGeneral
       , logRewrite
       , reachableNodesFrom
       , parents
       , topsort
       , operator
       , rootNodes
       , exposeDag
       , getExtras
       , updateExtras
       , insert
       , insertNoShare
       , replaceChild
       , relinkParents
       , relinkToNew
       , replace
       , pruneUnused
       , replaceRoot
       , infer
       ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           Debug.Trace
  
import           Database.Algebra.Dag.Common
import qualified Database.Algebra.Dag as Dag

-- | Cache some topological information about the DAG.
data Cache = Cache { cachedTopOrdering :: Maybe [AlgNode] } 
             
emptyCache :: Cache
emptyCache = Cache Nothing 

data RewriteState o e = RewriteState { nodeIDSupply   :: AlgNode          -- ^ Supply of fresh node ids
                                     , opMap          :: M.Map o AlgNode  -- ^ 
                                     , dag            :: Dag.AlgebraDag o -- ^ The DAG itself
                                     , cache          :: Cache            -- ^ Cache of some topological information
                                     , extras         :: e                -- ^ Polymorphic container for whatever needs to be provided additionally.
                                     , debugFlag      :: Bool             -- ^ Wether to output log messages via Debug.Trace.trace
                                     }
                      
-- | A Monad for DAG rewrites, parameterized over the type of algebra operators.
newtype Rewrite o e a = R (WriterT Log (State (RewriteState o e)) a) deriving (Monad, Functor, Applicative)
                                                                             
-- FIXME Map.findMax might call error
initRewriteState :: Ord o => Dag.AlgebraDag o -> e -> Bool -> RewriteState o e
initRewriteState d e debug =
    let maxIR = fst $ M.findMax $ Dag.nodeMap d
        om = M.foldrWithKey (\n o m -> M.insert o n m) M.empty $ Dag.nodeMap d
    in RewriteState { nodeIDSupply = maxIR + 1
                    , dag = d
                    , cache = emptyCache
                    , extras = e
                    , opMap = om
                    , debugFlag = debug
                    }
                                                               
-- | Run a rewrite action on the supplied graph. Returns the rewritten node map, the potentially
-- modified list of root nodes, the result of the rewrite and the rewrite log.
runRewrite :: Dag.Operator o => Rewrite o e r -> Dag.AlgebraDag o -> e -> Bool -> (Dag.AlgebraDag o, e, r, Log)
runRewrite (R m) d e debug = (dag s, extras s, res, rewriteLog) 
  where ((res, rewriteLog), s) = runState (runWriterT m) (initRewriteState d e debug)
        
-- | The log from a sequence of rewrite actions.
type Log = Seq.Seq String

-- | Return a fresh node id (only used internally).
freshNodeID :: Rewrite o e AlgNode
freshNodeID =
  R $ do
    s <- get
    let n = nodeIDSupply s
    put $ s { nodeIDSupply = n + 1 }
    return n
  
-- FIXME unwrapR should not be necessary: just provide a type alias for the monad stack
unwrapR :: Rewrite o e a -> WriterT Log (State (RewriteState o e)) a
unwrapR (R m) = m
                
invalidateCacheM :: Rewrite o e ()
invalidateCacheM =
  R $ do
    s <- get
    put $ s { cache = emptyCache }
  
-- internal helper function
putDag :: Dag.AlgebraDag o -> Rewrite o e ()
putDag d =
  R $ do
    s <- get
    put $ s { dag = d }
  
putCache :: Cache -> Rewrite o e ()
putCache c =
  R $ do
    s <- get
    put $ s { cache = c }
           
-- | Log a general message
logGeneral :: String -> Rewrite o e ()
logGeneral msg =  do
  d <- R $ gets debugFlag
  if d 
    then trace msg $ R $ tell $ Seq.singleton msg
    else R $ tell $ Seq.singleton msg

-- | Log a rewrite
logRewrite :: String -> AlgNode -> Rewrite o e ()
logRewrite rewrite node = 
  logGeneral $ "Triggering rewrite " ++ rewrite ++ " at node " ++ (show node)
         
-- | Return the set of nodes that are reachable from the specified node.
reachableNodesFrom :: AlgNode -> Rewrite o e (S.Set AlgNode)
reachableNodesFrom n =
  R $ do
    d <- gets dag
    return $ Dag.reachableNodesFrom n d
    
-- | Return the parents of a node
parents :: AlgNode -> Rewrite o e [AlgNode]
parents n = do
  parentNodes <- R $ gets ((Dag.parents n) . dag)
  filterM isReachable parentNodes

-- | Return a topological ordering of all reachable nodes in the DAG. 
topsort :: Dag.Operator o => Rewrite o e [AlgNode]
topsort = 
  R $ do
    s <- get
    let c = cache s
    case cachedTopOrdering c of
      Just o -> return o
      Nothing -> do
        let d = dag s
            ordering = Dag.topsort d
        unwrapR $ putCache $ c { cachedTopOrdering = Just ordering }
        return ordering

-- | Return the operator for a node id.
operator :: AlgNode -> Rewrite o e o
operator n = 
  R $ do
    d <- gets dag
    return $ Dag.operator n d

-- | Returns the root nodes of the DAG.
rootNodes :: Rewrite o e [AlgNode]
rootNodes = R $ liftM Dag.rootNodes $ liftM dag $ get

-- | Exposes the current state of the DAG
exposeDag :: Rewrite o e (Dag.AlgebraDag o)
exposeDag = R $ gets dag
          
getExtras :: Rewrite o e e
getExtras = R $ gets extras
          
updateExtras :: e -> Rewrite o e ()
updateExtras e =
  R $ do
    s <- get
    put $ s { extras = e }

-- | Insert an operator into the DAG and return its node id. If the operator is already
-- present (same op, same children), reuse it.
insert :: Dag.Operator o => o -> Rewrite o e AlgNode
insert op = 
  R $ do
    s <- get
    let om = opMap s
    case M.lookup op om of
      Just n  -> return n
      Nothing -> do
                   n <- unwrapR freshNodeID
                   unwrapR invalidateCacheM 
                   unwrapR $ putDag $ Dag.insert n op $ dag s
                   s' <- get
                   put $ s' { opMap = M.insert op n om }
                   return n
                   
-- | Insert an operator into the DAG and return its node id WITHOUT reusing an
-- operator if it is already present.
insertNoShare :: Dag.Operator o => o -> Rewrite o e AlgNode
insertNoShare op =
  R $ do
    s <- get
    n <- unwrapR freshNodeID
    unwrapR $ putDag $ Dag.insert n op $ dag s
    return n

-- | replaceChildM n old new replaces all links from node n to node old with links
--   to node new 
replaceChild :: Dag.Operator o => AlgNode -> AlgNode -> AlgNode -> Rewrite o e ()
replaceChild n old new = 
  R $ do
    s <- get
    unwrapR invalidateCacheM
    unwrapR $ putDag $ Dag.replaceChild n old new $ dag s
 
-- | relinkParents old new replaces _all_ links to old with links to new
relinkParents :: Dag.Operator o => AlgNode -> AlgNode -> Rewrite o e ()
relinkParents old new = do
  ps <- parents old
  forM_ ps $ (\p -> replaceChild p old new)

-- | Creates a new node from the operator and replaces the old node with it
-- by rewireing all links to the old node.
relinkToNew :: Dag.Operator o => AlgNode -> o -> Rewrite o e AlgNode
relinkToNew oldNode newOp = do
  newNode <- insert newOp
  rs <- rootNodes
  relinkParents oldNode newNode
  if oldNode `elem` rs
    then replaceRoot oldNode newNode
    else return ()
  return newNode

-- | Replaces the operator at the specified node id with a new operator.
replace :: Dag.Operator o => AlgNode -> o -> Rewrite o e ()
replace node newOp =
  R $ do
    d <- gets dag
    unwrapR invalidateCacheM
    unwrapR $ putDag $ Dag.replace node newOp d

-- | Remove all unreferenced nodes from the DAG: all nodes are unreferenced which
-- are not reachable from one of the root nodes.
pruneUnused :: Rewrite o e ()
pruneUnused =
  R $ do
    s <- get
    case Dag.pruneUnused $ dag s of
      Just dag' -> do
        unwrapR invalidateCacheM
        unwrapR $ putDag dag'
      Nothing -> return ()

-- Returns true iff the given node is reachable in the DAG
isReachable :: AlgNode -> Rewrite o e Bool
isReachable n =
  R $ do
    d <- gets dag
    let nodes = Dag.reachableNodes d
    return $ S.member n nodes
  
-- | Replaces an entry in the list of root nodes.
replaceRoot :: AlgNode -> AlgNode -> Rewrite o e ()
replaceRoot oldRoot newRoot = do
  R $ do 
    s <- get
    if not $ M.member newRoot $ Dag.nodeMap $ dag s
      then error "replaceRootM: new root node is not present in the DAG"
      else unwrapR $ putDag $ Dag.replaceRoot (dag s) oldRoot newRoot
 
-- | Apply a pure function to the DAG.
infer :: (Dag.AlgebraDag o -> b) -> Rewrite o e b
infer f = R $ liftM f $ gets dag
