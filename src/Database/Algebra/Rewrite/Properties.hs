module Database.Algebra.Rewrite.Properties(inferBottomUpGeneral) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.IntMap                 as M
import           Database.Algebra.Dag
import           Database.Algebra.Dag.Common

-- | Inference of bottom up properties p over a DAG of operator type o.
type Inference p o a = StateT (NodeMap p) (Reader (AlgebraDag o)) a

hasBeenVisited :: AlgNode -> Inference p o Bool
hasBeenVisited n = do
  pm <- get
  return $ M.member n pm

putProperty :: AlgNode -> p -> Inference p o ()
putProperty n p = do
  pm <- get
  put $ M.insert n p pm


traverse :: (Show o, Operator o) => (o -> AlgNode -> NodeMap p -> p) -> AlgNode -> Inference p o ()
traverse inferWorker n = do
  visited <- hasBeenVisited n
  if visited
    then return ()
    else do
      dag <- lift ask
      let op = operator n dag
      mapM_ (traverse inferWorker) (opChildren op)
      pm <- get
      putProperty n (inferWorker op n pm)

-- | Infer bottom up properties with the given inference function.
inferBottomUpGeneral :: Operator o
                        => (o -> AlgNode -> NodeMap p -> p)  -- ^ Function that infers properties for a single node
                        -> AlgebraDag o                   -- ^ The DAG
                        -> NodeMap p                      -- ^ The final mapping from nodes to properties
inferBottomUpGeneral inferWorker dag = runReader (execStateT infer M.empty) dag
  where infer = mapM_ (traverse inferWorker) (rootNodes dag)
