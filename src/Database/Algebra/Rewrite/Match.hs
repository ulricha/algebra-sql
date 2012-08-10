{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies  #-}

module Database.Algebra.Rewrite.Match 
       ( DefaultMatch(..)
       , DagMatch(..)
       , runDefaultMatch
       , pattern
       , v ) where

import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Applicative
  
import Database.Algebra.Dag.Common
import qualified Database.Algebra.Dag as Dag
import Database.Algebra.Rewrite.PatternConstruction(pattern, v)
  
class Monad m => (DagMatch m) o p | m -> o p where
  -- | Returns the parents of a node in a DefaultMatch context.
  getParents :: AlgNode -> m [AlgNode]

  getOperator :: AlgNode -> m o
  hasPath :: AlgNode -> AlgNode -> m Bool
  getRootNodes :: m [AlgNode]
  -- | Fails the complete match if the predicate is False.
  predicate :: Bool -> m ()
  -- | Fails the complete match if the value is Nothing
  try :: Maybe a -> m a
  -- | Runs the supplied Match action on the operator that belongs to the given node.
  matchOp :: AlgNode -> (o -> m a) -> m a
  -- | Look up the properties for a given node.
  properties :: AlgNode -> m p
  
data Env o p = Env { dag :: Dag.AlgebraDag o
                   , propMap :: NodeMap p }

-- | The Match monad models the failing of a match and provides limited read-only access
-- to the DAG.
newtype DefaultMatch o p a = M (MaybeT (Reader (Env o p)) a) deriving (Monad, Functor, Applicative)

-- | Runs a match on the supplied DAG. If the Match fails, 'Nothing' is returned.
-- If the Match succeeds, it returns just the result.
runDefaultMatch :: Dag.AlgebraDag o -> NodeMap p -> DefaultMatch o p a -> Maybe a
runDefaultMatch d pm (M match) = runReader (runMaybeT match) env
  where env = Env { dag = d, propMap = pm }
     
instance DagMatch (DefaultMatch o p) o p where
           
  getParents q = M $ asks ((Dag.parents q) . dag)
            
  getOperator q = M $ asks ((Dag.operator q) . dag)

  hasPath q1 q2 = M $ asks ((Dag.hasPath q1 q2) . dag)
                
  getRootNodes = M $ asks (Dag.rootNodes . dag)

  predicate True    = M $ return ()
  predicate False   = M $ fail ""
               
  try (Just x) = return x
  try Nothing  = fail ""
                    
  matchOp q match = M $ asks ((Dag.operator q) . dag) >>= (\o -> unwrap $ match o)
    where unwrap (M r) = r

  properties q = do
    M $ do 
      pm <- asks propMap
      case M.lookup q pm of
        Just p -> return p
        Nothing -> error $ "DefaultMatch.properties: no properties for node " ++ (show q)
