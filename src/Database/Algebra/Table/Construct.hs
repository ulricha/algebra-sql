{-# LANGUAGE FlexibleContexts #-}

-- | This module contains smart constructors for table algebra plans.
module Database.Algebra.Table.Construct
    ( -- * Value and type constructors
      int, string, bool, double, dec, date
    , intT, stringT, boolT, decT, doubleT, dateT
      -- * Smart constructors for algebraic operators
    , dbTable, litTable, litTable', thetaJoin
    , semiJoin, antiJoin, rank, difference, rowrank
    , select, distinct, cross, union, proj, aggr, winFun
    , rownum, rownum', leftOuterJoin
      -- * Lifted smart constructors for table algebra operators
    , thetaJoinM, semiJoinM, antiJoinM, rankM, differenceM
    , rowrankM, selectM, distinctM, crossM, unionM, projM
    , aggrM, winFunM, rownumM, rownum'M, leftOuterJoinM
    ) where

import           Control.Monad.State
import           Data.Scientific
import qualified Data.Text                   as T
import qualified Data.Time.Calendar          as C
import           Database.Algebra.Dag.Build
import           Database.Algebra.Dag.Common
import           Database.Algebra.Table.Lang

--------------------------------------------------------------------------------
-- Value constructors

-- | Create a TA int value
int :: Integer -> AVal
int = VInt

-- | Create a TA string value
string :: T.Text -> AVal
string = VStr

-- | Create a TA boolean value
bool :: Bool -> AVal
bool = VBool

-- | Create a TA double value
double :: Double -> AVal
double = VDouble

-- | Create a TA decimal value
dec :: Scientific -> AVal
dec = VDec

date :: C.Day -> AVal
date = VDate . Date

-- | Types of atomic  values
intT, stringT, boolT, decT, doubleT, dateT :: ATy
intT    = AInt
stringT = AStr
boolT   = ABool
decT    = ADec
doubleT = ADouble
dateT   = ADate

--------------------------------------------------------------------------------
-- Smart constructors for algebraic operators

-- | Construct a database table node
-- The first argument is the \emph{qualified} name of the database
-- table. The second describes the columns in alphabetical order.
-- The third argument describes the database keys (one table key can
-- span over multiple columns).
dbTable :: MonadState (BuildState TableAlgebra) m => String -> [(Attr, ATy)] -> [Key] -> m AlgNode
dbTable n cs ks = insert $ NullaryOp $ TableRef (n, cs, ks)

-- | Construct a table with one value
litTable :: MonadState (BuildState TableAlgebra) m => AVal -> String -> ATy -> m AlgNode
litTable v s t = insert $ NullaryOp $ LitTable ([[v]], [(s, t)])

-- | Construct a literal table with multiple columns and rows
litTable' :: MonadState (BuildState TableAlgebra) m => [[AVal]] -> [(String, ATy)] -> m AlgNode
litTable' v s = insert $ NullaryOp $ LitTable (v, s)

thetaJoin :: MonadState (BuildState TableAlgebra) m => [(Expr, Expr, JoinRel)] -> AlgNode -> AlgNode -> m AlgNode
thetaJoin cond c1 c2 = insert $ BinOp (ThetaJoin cond) c1 c2

leftOuterJoin :: MonadState (BuildState TableAlgebra) m => [(Expr, Expr, JoinRel)] -> AlgNode -> AlgNode -> m AlgNode
leftOuterJoin cond c1 c2 = insert $ BinOp (LeftOuterJoin cond) c1 c2

semiJoin :: MonadState (BuildState TableAlgebra) m => [(Expr, Expr, JoinRel)] -> AlgNode -> AlgNode -> m AlgNode
semiJoin cond c1 c2 = insert $ BinOp (SemiJoin cond) c1 c2

antiJoin :: MonadState (BuildState TableAlgebra) m => [(Expr, Expr, JoinRel)] -> AlgNode -> AlgNode -> m AlgNode
antiJoin cond c1 c2 = insert $ BinOp (AntiJoin cond) c1 c2

-- | Assign a number to each row in column 'ResAttr' incrementally
-- sorted by `sort'. The numbering is not dense!
rank :: MonadState (BuildState TableAlgebra) m => ResAttr -> [SortSpec] -> AlgNode -> m AlgNode
rank res sort c1 = insert $ UnOp (Rank (res, sort)) c1

-- | Compute the difference between two plans.
difference :: MonadState (BuildState TableAlgebra) m => AlgNode -> AlgNode -> m AlgNode
difference q1 q2 = insert $ BinOp (Difference ()) q1 q2

-- | Same as rank but provides a dense numbering.
rowrank :: MonadState (BuildState TableAlgebra) m => ResAttr -> [SortSpec] -> AlgNode -> m AlgNode
rowrank res sort c1 = insert $ UnOp (RowRank (res, sort)) c1

-- | Select rows where the column `SelAttr' contains True.
select :: MonadState (BuildState TableAlgebra) m => Expr -> AlgNode -> m AlgNode
select sel c1 = insert $ UnOp (Select sel) c1

-- | Remove duplicate rows
distinct :: MonadState (BuildState TableAlgebra) m => AlgNode -> m AlgNode
distinct c1 = insert $ UnOp (Distinct ()) c1

-- | Make cross product from two plans
cross :: MonadState (BuildState TableAlgebra) m => AlgNode -> AlgNode -> m AlgNode
cross c1 c2 = insert $ BinOp (Cross ()) c1 c2

-- | Union between two plans
union :: MonadState (BuildState TableAlgebra) m => AlgNode -> AlgNode -> m AlgNode
union c1 c2 = insert $ BinOp (DisjUnion ()) c1 c2

-- | Project/rename certain column out of a plan
proj :: MonadState (BuildState TableAlgebra) m => [Proj] -> AlgNode -> m AlgNode
proj ps c = insert $ UnOp (Project ps) c

-- | Apply aggregate functions to a plan
aggr :: MonadState (BuildState TableAlgebra) m => [(AggrType, ResAttr)] -> [(Attr, Expr)] -> AlgNode -> m AlgNode
aggr aggrs part c1 = insert $ UnOp (Aggr (aggrs, part)) c1

winFun :: MonadState (BuildState TableAlgebra) m => (ResAttr, WinFun)
       -> [PartExpr]
       -> [SortSpec]
       -> Maybe FrameBounds
       -> AlgNode
       -> m AlgNode
winFun fun part sort frame c = insert $ UnOp (WinFun (fun, part, sort, frame)) c

-- | Similar to rowrank but this will assign a unique number to every
-- row (even if two rows are equal)
rownum :: MonadState (BuildState TableAlgebra) m => Attr -> [Attr] -> [PartExpr] -> AlgNode -> m AlgNode
rownum res sort part c1 = insert $ UnOp (RowNum (res, map (\c -> (ColE c, Asc)) sort, part)) c1

-- | Same as rownum but columns can be assigned an ordering direction
rownum' :: MonadState (BuildState TableAlgebra) m => Attr -> [SortSpec] -> [PartExpr] -> AlgNode -> m AlgNode
rownum' res sort part c1 = insert $ UnOp (RowNum (res, sort, part)) c1

--------------------------------------------------------------------------------
-- Lifted smart constructors for table algebra operators

bind1 :: Monad m => (a -> m b) -> m a -> m b
bind1 = (=<<)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do
    a' <- a
    b' <- b
    f a' b'

-- | Perform theta join on two plans
thetaJoinM :: [(Expr, Expr, JoinRel)] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
thetaJoinM cond = bind2 (thetaJoin cond)

leftOuterJoinM :: [(Expr, Expr, JoinRel)] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
leftOuterJoinM cond = bind2 (leftOuterJoin cond)

-- | Perform a semi join on two plans
semiJoinM :: [(Expr, Expr, JoinRel)] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
semiJoinM cond = bind2 (semiJoin cond)

-- | Perform an anti join on two plans
antiJoinM :: [(Expr, Expr, JoinRel)] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
antiJoinM cond = bind2 (antiJoin cond)

-- | Assign a number to each row in column 'ResAttr' incrementing
-- sorted by `sort'. The numbering is not dense!
rankM :: ResAttr -> [SortSpec] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
rankM res sort = bind1 (rank res sort)

-- | Compute the difference between two plans.
differenceM :: Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
differenceM = bind2 difference

-- | Same as rank but provides a dense numbering.
rowrankM :: ResAttr -> [SortSpec] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
rowrankM res sort = bind1 (rowrank res sort)

-- | Select rows where the column `SelAttr' contains True.
selectM :: Expr -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
selectM sel = bind1 (select sel)

-- | Remove duplicate rows
distinctM :: Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
distinctM = bind1 distinct

-- | Make cross product from two plans
crossM :: Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
crossM = bind2 cross

-- | Union between two plans
unionM :: Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
unionM = bind2 union

-- | Project/rename certain column out of a plan
projM :: [Proj] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
projM cols = bind1 (proj cols)

-- | Apply aggregate functions to a plan
aggrM :: [(AggrType, ResAttr)] -> [(Attr, Expr)] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
aggrM aggrs part = bind1 (aggr aggrs part)

winFunM :: (ResAttr, WinFun)
        -> [PartExpr]
        -> [SortSpec]
        -> Maybe FrameBounds
        -> Build TableAlgebra AlgNode
        -> Build TableAlgebra AlgNode
winFunM fun part sort frame = bind1 (winFun fun part sort frame)

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownumM :: Attr -> [Attr] -> [PartExpr] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
rownumM res sort part = bind1 (rownum res sort part)

-- | Same as rownum but columns can be assigned an ordering direction
rownum'M :: Attr -> [SortSpec] -> [PartExpr] -> Build TableAlgebra AlgNode -> Build TableAlgebra AlgNode
rownum'M res sort part = bind1 (rownum' res sort part)
