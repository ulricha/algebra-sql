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
       , operatorSafe
       , rootNodes
       , exposeDag
       , getExtras
       , updateExtras
       , insert
       , insertNoShare
       , replaceChild
       , replace
       , replaceWithNew
       , replaceRoot
       , infer
       , collect
       ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.IntMap                 as IM
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import           Debug.Trace
import Text.Printf

import qualified Database.Algebra.Dag        as Dag
import           Database.Algebra.Dag.Common

-- | Cache some topological information about the DAG.
data Cache = Cache { cachedTopOrdering :: Maybe [AlgNode] }

emptyCache :: Cache
emptyCache = Cache Nothing

data RewriteState o e = RewriteState
  { dag            :: Dag.AlgebraDag o -- ^ The DAG itself
  , cache          :: Cache            -- ^ Cache of some topological information
  , extras         :: e                -- ^ Polymorphic container for whatever needs to be provided additionally.
  , debugFlag      :: Bool             -- ^ Wether to output log messages via Debug.Trace.trace
  , collectNodes   :: S.Set AlgNode    -- ^ List of nodes which must be checked during garbage collection
  }

-- | A Monad for DAG rewrites, parameterized over the type of algebra operators.
newtype Rewrite o e a = R (WriterT Log (State (RewriteState o e)) a) deriving (Monad, Functor, Applicative)

-- FIXME Map.findMax might call error
initRewriteState :: (Ord o, Dag.Operator o) => Dag.AlgebraDag o -> e -> Bool -> RewriteState o e
initRewriteState d e debug =
    RewriteState { dag = d
                 , cache = emptyCache
                 , extras = e
                 , debugFlag = debug
                 , collectNodes = S.empty
                 }

-- | Run a rewrite action on the supplied graph. Returns the rewritten node map, the potentially
-- modified list of root nodes, the result of the rewrite and the rewrite log.
runRewrite :: Dag.Operator o => Rewrite o e r -> Dag.AlgebraDag o -> e -> Bool -> (Dag.AlgebraDag o, e, r, Log)
runRewrite (R m) d e debug = (dag s, extras s, res, rewriteLog)
  where ((res, rewriteLog), s) = runState (runWriterT m) (initRewriteState d e debug)

-- | The log from a sequence of rewrite actions.
type Log = Seq.Seq String

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
parents n = R $ gets ((Dag.parents n) . dag)

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
operator :: Dag.Operator o => AlgNode -> Rewrite o e o
operator n =
  R $ do
    d <- gets dag
    return $ Dag.operator n d

operatorSafe :: AlgNode -> Rewrite o e (Maybe o)
operatorSafe n =
  R $ do
    d <- gets dag
    return $ IM.lookup n (Dag.nodeMap d)

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
insert :: (Dag.Operator o, Show o) => o -> Rewrite o e AlgNode
insert op =
  R $ do
    d <- gets dag
    unwrapR invalidateCacheM
    let (n, d') = Dag.insert op d
    unwrapR $ putDag d'
    return n

-- | Insert an operator into the DAG and return its node id WITHOUT reusing an
-- operator if it is already present.
insertNoShare :: Dag.Operator o => o -> Rewrite o e AlgNode
insertNoShare op =
  R $ do
    d <- gets dag
    unwrapR invalidateCacheM
    let (n, d') = Dag.insertNoShare op d
    unwrapR $ putDag d'
    return n

-- | replaceChildM n old new replaces all links from node n to node old with links
--   to node new
replaceChild :: Dag.Operator o => AlgNode -> AlgNode -> AlgNode -> Rewrite o e ()
replaceChild n old new =
  R $ do
    s <- get
    unwrapR invalidateCacheM
    unwrapR $ putDag $ Dag.replaceChild n old new $ dag s

-- | replace old new replaces _all_ links to old with links to new
replace :: Dag.Operator o => AlgNode -> AlgNode -> Rewrite o e ()
replace old new = do
  trace (printf "replace %d %d" old new) $ return ()
  d  <- exposeDag
  when (old == 22) $ trace (show $ Dag.nodeMap d) $ return ()
  when (old == 22) $ trace (show $ Dag.refCountMap d) $ return ()

  ps <- parents old
  forM_ ps $ (\p -> replaceChild p old new)
  addCollectNode old
  R $ do s <- get
         unwrapR $ putDag $ Dag.replaceRoot (dag s) old new

-- | Creates a new node from the operator and replaces the old node with it
-- by rewiring all links to the old node.
replaceWithNew :: (Dag.Operator o, Show o) => AlgNode -> o -> Rewrite o e AlgNode
replaceWithNew oldNode newOp = do
  newNode <- insert newOp
  trace (printf "%d -> %d(%s)" oldNode newNode (show newOp)) $ return ()                     
  d <- Dag.refCountMap <$> exposeDag
  trace (show d) $ return ()
  replace oldNode newNode
  return newNode

-- | Apply a pure function to the DAG.
infer :: (Dag.AlgebraDag o -> b) -> Rewrite o e b
infer f = R $ liftM f $ gets dag

addCollectNode :: AlgNode -> Rewrite o e ()
addCollectNode n =
  R $ do
    s <- get
    put $ s { collectNodes = S.insert n $ collectNodes s }

collect :: (Show o, Dag.Operator o) => Rewrite o e ()
collect =
  R $ do
    s <- get
    let d' = Dag.collect (collectNodes s) (dag s)
    put s { dag = d', collectNodes = S.empty }

replaceRoot :: Dag.Operator o => AlgNode -> AlgNode -> Rewrite o e ()
replaceRoot oldRoot newRoot =
  R $ do
    s <- get
    if not $ IM.member newRoot $ Dag.nodeMap $ dag s
      then error "replaceRootM: new root node is not present in the DAG"
      else unwrapR $ putDag $ Dag.replaceRoot (dag s) oldRoot newRoot
