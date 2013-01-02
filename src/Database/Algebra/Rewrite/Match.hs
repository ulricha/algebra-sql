{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Algebra.Rewrite.Match
       ( Match(..)
       , runMatch
       , getParents
       , getOperator
       , hasPath
       , getRootNodes
       , predicate
       , try
       , matchOp
       , lookupExtras
       , exposeEnv
       , properties) where

import qualified Data.IntMap                 as M

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import qualified Database.Algebra.Dag        as Dag
import           Database.Algebra.Dag.Common

data Env o p e = Env { dag :: Dag.AlgebraDag o
                     , propMap :: NodeMap p
                     , extras :: e }

-- | The Match monad models the failing of a match and provides limited read-only access
-- to the DAG.
newtype Match o p e a = M (MaybeT (Reader (Env o p e)) a) deriving (Monad, Functor, Applicative)

-- | Runs a match on the supplied DAG. If the Match fails, 'Nothing' is returned.
-- If the Match succeeds, it returns just the result.
runMatch :: e -> Dag.AlgebraDag o -> NodeMap p -> Match o p e a -> Maybe a
runMatch e d pm (M match) = runReader (runMaybeT match) env
  where env = Env { dag = d, propMap = pm, extras = e }

-- | Returns the parents of a node in a Match context.
getParents :: AlgNode -> Match o p e [AlgNode]
getParents q = do
  M $ asks ((Dag.parents q) . dag)

getOperator :: Dag.Operator o => AlgNode -> Match o p e o
getOperator q = M $ asks ((Dag.operator q) . dag)

hasPath :: AlgNode -> AlgNode -> Match o p e Bool
hasPath q1 q2 = M $ asks ((Dag.hasPath q1 q2) . dag)

getRootNodes :: Match o p e [AlgNode]
getRootNodes = M $ asks (Dag.rootNodes . dag)

-- | Fails the complete match if the predicate is False.
predicate :: Bool -> Match o p e ()
predicate True    = M $ return ()
predicate False   = M $ fail ""

-- | Fails the complete match if the value is Nothing
try :: Maybe a -> Match o p e a
try (Just x) = return x
try Nothing  = fail ""

-- | Runs the supplied Match action on the operator that belongs to the given node.
matchOp :: Dag.Operator o => AlgNode -> (o -> Match o p e a) -> Match o p e a
matchOp q match = M $ asks ((Dag.operator q) . dag) >>= (\o -> unwrap $ match o)
  where unwrap (M r) = r

-- | Look up the properties for a given node.
properties :: AlgNode -> Match o p e p
properties q = do
  M $ do
    pm <- asks propMap
    case M.lookup q pm of
      Just p -> return p
      Nothing -> error $ "Match.properties: no properties for node " ++ (show q)

lookupExtras :: Match o p e e
lookupExtras = M $ asks extras

exposeEnv :: Match o p e (Dag.AlgebraDag o, NodeMap p, e)
exposeEnv = M $ do
  env <- ask
  return (dag env, propMap env, extras env)
