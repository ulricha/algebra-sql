-- | This module exports monadic combinators for creating graphs
module Database.Algebra.Pathfinder.Monadic.Create 
  ( eqJoinM
  , rankM
  , differenceM
  , rowrankM
  , posSelectM
  , selectM
  , distinctM
  , crossM
  , unionM
  , projM
  , aggrM
  , rownumM
  , rownum'M
  , thetaJoinM
  , semiJoinM
  , antiJoinM
  ) where

import           Database.Algebra.Dag.Builder
import           Database.Algebra.Dag.Common
import           Database.Algebra.Pathfinder.Data.Algebra
import qualified Database.Algebra.Pathfinder.Data.Create  as C

bind1 :: Monad m => (a -> m b) -> m a -> m b
bind1 = (=<<)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do
                a' <- a
                b' <- b
                f a' b'

-- | Perform theta join on two plans
thetaJoinM :: SemInfJoin -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
thetaJoinM cond = bind2 (C.thetaJoin cond)

-- | Perform a semi join on two plans
semiJoinM :: SemInfJoin -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
semiJoinM cond = bind2 (C.semiJoin cond)

-- | Perform an anti join on two plans
antiJoinM :: SemInfJoin -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
antiJoinM cond = bind2 (C.antiJoin cond)

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoinM :: String -> String -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
eqJoinM n1 n2 = bind2 (C.eqJoin n1 n2)

-- | Assign a number to each row in column 'ResAttrName' incrementing
-- sorted by `SortInf'. The numbering is not dense!
rankM :: ResAttrName -> SortInf -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
rankM res sort = bind1 (C.rank res sort)

-- | Compute the difference between two plans.
differenceM :: GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
differenceM = bind2 C.difference

-- | Same as rank but provides a dense numbering.
rowrankM :: ResAttrName -> SortInf -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
rowrankM res sort = bind1 (C.rowrank res sort)

-- | Get's the nth element(s) of a (partitioned) table.
posSelectM :: Int -> SortInf -> Maybe AttrName -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
posSelectM n sort part = bind1 (C.posSelect n sort part)

-- | Select rows where the column `SelAttrName' contains True.
selectM :: Expr -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
selectM sel = bind1 (C.select sel)

-- | Remove duplicate rows
distinctM :: GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
distinctM = bind1 C.distinct

-- | Make cross product from two plans
crossM :: GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
crossM = bind2 C.cross

-- | Union between two plans
unionM :: GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
unionM = bind2 C.union

-- | Project/rename certain column out of a plan
projM :: [Proj] -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
projM cols = bind1 (C.proj cols)

-- | Apply aggregate functions to a plan
aggrM :: [(AggrType, ResAttrName)] -> [(AttrName, Expr)] -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
aggrM aggrs part = bind1 (C.aggr aggrs part)

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownumM :: AttrName -> [AttrName] -> Maybe AttrName -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
rownumM res sort part = bind1 (C.rownum res sort part)

-- | Same as rownum but columns can be assigned an ordering direction
rownum'M :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
rownum'M res sort part = bind1 (C.rownum' res sort part)
