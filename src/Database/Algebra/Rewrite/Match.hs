{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Algebra.Rewrite.Match 
       ( Match
       , runMatch
       , parents
       , predicate
       , predicateM
       , notM
       , (<&&>)
       , (<||>)
       , properties
       , matchOp
       , m
       , v ) where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
  
import Database.Algebra.Dag.Common
import qualified Database.Algebra.Dag as Dag
import Database.Algebra.Rewrite.PatternConstruction(m, v)
  
data Env o p = Env { dag :: Dag.AlgebraDag o
                   , propMap :: NodeMap p }

-- | The Match monad models the failing of a match and provides limited read-only access
-- to the DAG.
newtype Match o p a = M (MaybeT (Reader (Env o p)) a) deriving Monad
     
-- | Runs a match on the supplied DAG. If the Match fails, 'Nothing' is returned.
-- If the Match succeeds, it returns just the result.
runMatch :: Dag.AlgebraDag o -> NodeMap p -> Match o p a -> Maybe a
runMatch d pm (M match) = runReader (runMaybeT match) env
  where env = Env { dag = d, propMap = pm }
           
-- | Returns the parents of a node in a Match context.
parents :: AlgNode -> Match o p [AlgNode]
parents q = M $ asks ((Dag.parents q) . dag)

-- | Fails the complete match if the predicate is False.
predicate :: Bool -> Match o p ()
predicate True    = M $ return ()
predicate False   = M $ fail ""
               
predicateM :: Match o p Bool -> Match o p ()
predicateM match = do
  b <- match
  if b
    then return ()
    else fail ""
                    
-- | Runs the supplied Match action on the operator that belongs to the given node.
matchOp :: Dag.Operator o => AlgNode -> (o -> Match o p a) -> Match o p a
matchOp q match = M $ asks ((Dag.operator q) . dag) >>= (\o -> unwrap $ match o)
  where unwrap (M r) = r

-- | Look up the properties for a given node.
properties :: AlgNode -> Match o p p
properties q = do
  M $ do 
    pm <- asks propMap
    case M.lookup q pm of
      Just p -> return p
      Nothing -> error $ "Match.properties: no properties for node " ++ (show q)

-- | Monadic boolean 'or' operator
notM :: Monad m => m Bool -> m Bool
notM = liftM not
          
-- | Monadic boolean 'and' operator
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)
         
-- | Monadic boolean 'or' operator
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

