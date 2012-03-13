module Database.Algebra.Rewrite.Match 
       ( Match
       , runMatch
       , parents
       , predicate
       , properties
       , match) where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
  
import Database.Algebra.Dag.Common
import qualified Database.Algebra.Dag as Dag
  
data Env o p = Env { dag :: Dag.AlgebraDag o
                   , propMap :: NodeMap p }

-- | The Match monad models the failing of a match and provides limited read-only access
-- to the DAG.
newtype Match o p a = M (MaybeT (Reader (Env o p)) a)
     
-- | Runs a match on the supplied DAG. If the Match fails, 'Nothing' is returned.
-- If the Match succeeds, it returns just the result.
runMatch :: Dag.AlgebraDag o -> NodeMap p -> Match o p a -> Maybe a
runMatch d pm (M m) = runReader (runMaybeT m) env
  where env = Env { dag = d, propMap = pm }
           
-- | Returns the parents of a node in a Match context.
parents :: AlgNode -> Match o p [AlgNode]
parents q = M $ asks ((Dag.parents q) . dag)

-- | Fails the complete match if the predicate is False.
predicate :: Bool -> Match o p ()
predicate True    = M $ return ()
predicate False   = M $ fail ""
                    
-- | Runs the supplied Match action on the operator that belongs to the given node.
match :: Dag.Operator o => AlgNode -> (o -> Match o p a) -> Match o p a
match q m = M $ asks ((Dag.operator q) . dag) >>= (\o -> unwrap $ m o)
  where unwrap (M r) = r

-- | Look up the properties for a given node.
properties :: AlgNode -> Match o p p
properties q = do
  M $ do 
    pm <- asks propMap
    case M.lookup q pm of
      Just p -> return p
      Nothing -> error $ "Match.properties: no properties for node " ++ (show q)
