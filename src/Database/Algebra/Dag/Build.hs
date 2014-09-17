{-# LANGUAGE GADTs #-}

module Database.Algebra.Dag.Build
    ( Build
    , runBuild
    , tagM
    , insert
    , insertNoShare
    ) where

import           Control.Monad.State
import qualified Data.IntMap                 as IM

import qualified Database.Algebra.Dag        as Dag
import           Database.Algebra.Dag.Common


data BuildState alg = BuildState
    { dag  :: Dag.AlgebraDag alg  -- ^ The operator DAG that is built
    , tags :: NodeMap [Tag]       -- ^ Tags for nodes
    }

-- | The DAG builder monad, abstracted over the algebra stored in the
-- DAG. Internally, the monad detects sharing of subgraphs via hash
-- consing.
type Build alg = State (BuildState alg)

-- | Evaluate the monadic graph into an algebraic plan, given a loop
-- relation.
runBuild :: Build alg r -> (Dag.AlgebraDag alg, r, NodeMap [Tag])
runBuild m = (dag s, r, tags s)
  where 
    initialBuildState = BuildState { dag = Dag.emptyDag, tags = IM.empty }
    (r, s)            = runState m initialBuildState

-- | Tag a subtree with a comment
tag :: String -> AlgNode -> Build alg AlgNode
tag msg c = do
    modify $ \s -> s { tags = IM.insertWith (++) c [msg] $ tags s }
    return c

-- | Tag a subtree with a comment (monadic version)
tagM :: String -> Build alg AlgNode -> Build alg AlgNode
tagM s = (=<<) (tag s)

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.
insert :: Dag.Operator alg => alg -> Build alg AlgNode
insert op = do
    d <- gets dag
    let (n, d') = Dag.insert op d
    modify $ \s -> s { dag = d' }
    return n

insertNoShare :: Dag.Operator alg => alg -> Build alg AlgNode
insertNoShare op = do
    d <- gets dag
    let (n, d') = Dag.insertNoShare op d
    modify $ \s -> s { dag = d' }
    return n
