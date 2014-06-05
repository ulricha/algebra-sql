-- | This module contains smart constructors for table algebra plans.
module Database.Algebra.Table.Construct
    ( -- * Value and type constructors
      int, string, bool, double, dec, nat
    , intT, stringT, boolT, decT, doubleT, natT
      -- * Smart constructors for algebraic operators
    , dbTable, litTable, litTable', eqJoin, thetaJoin
    , semiJoin, antiJoin, rank, difference, rowrank
    , select, distinct, cross, union, proj, aggr
    , rownum, rownum'
      -- * Lifted smart constructors for table algebra operators
    , thetaJoinM, semiJoinM, antiJoinM, eqJoinM, rankM, differenceM
    , rowrankM, selectM, distinctM, crossM, unionM, projM
    , aggrM, rownumM, rownum'M
    ) where

import           Database.Algebra.Dag.Builder
import           Database.Algebra.Dag.Common
import           Database.Algebra.Table.Lang

--------------------------------------------------------------------------------
-- Value constructors

-- | Create a TA int value
int :: Integer -> AVal
int = VInt

-- | Create a TA string value
string :: String -> AVal
string = VStr

-- | Create a TA boolean value
bool :: Bool -> AVal
bool = VBool

-- | Create a TA double value
double :: Double -> AVal
double = VDouble

-- | Create a TA decimal value
dec :: Float -> AVal
dec = VDec

-- | Create a TA nat value
nat :: Integer -> AVal
nat = VNat

-- | Types of atomic  values
intT, stringT, boolT, decT, doubleT, natT :: ATy
intT    = AInt
stringT = AStr
boolT   = ABool
decT    = ADec
doubleT = ADouble
natT    = ANat

--------------------------------------------------------------------------------
-- Smart constructors for algebraic operators

-- | Construct a database table node
-- The first argument is the \emph{qualified} name of the database
-- table. The second describes the columns in alphabetical order.
-- The third argument describes the database keys (one table key can
-- span over multiple columns).
dbTable :: String -> [(AttrName, ATy)] -> [Key] -> GraphM a TableAlgebra AlgNode
dbTable n cs ks = insertNode $ NullaryOp $ TableRef (n, cs, ks)

-- | Construct a table with one value
litTable :: AVal -> String -> ATy -> GraphM a TableAlgebra AlgNode
litTable v s t = insertNode $ NullaryOp $ LitTable [[v]] [(s, t)]

-- | Construct a literal table with multiple columns and rows
litTable' :: [[AVal]] -> [(String, ATy)] -> GraphM a TableAlgebra AlgNode
litTable' v s = insertNode $ NullaryOp $ LitTable v s

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoin :: LeftAttrName -> RightAttrName -> AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
eqJoin n1 n2 c1 c2 = insertNode $ BinOp (EqJoin (n1, n2)) c1 c2

thetaJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
thetaJoin cond c1 c2 = insertNode $ BinOp (ThetaJoin cond) c1 c2

semiJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
semiJoin cond c1 c2 = insertNode $ BinOp (SemiJoin cond) c1 c2

antiJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
antiJoin cond c1 c2 = insertNode $ BinOp (AntiJoin cond) c1 c2

-- | Assign a number to each row in column 'ResAttrName' incrementally
-- sorted by `sort'. The numbering is not dense!
rank :: ResAttrName -> [SortAttr] -> AlgNode -> GraphM a TableAlgebra AlgNode
rank res sort c1 = insertNode $ UnOp (Rank (res, sort)) c1

-- | Compute the difference between two plans.
difference :: AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
difference q1 q2 = insertNode $ BinOp (Difference ()) q1 q2

-- | Same as rank but provides a dense numbering.
rowrank :: ResAttrName -> [SortAttr] -> AlgNode -> GraphM a TableAlgebra AlgNode
rowrank res sort c1 = insertNode $ UnOp (RowRank (res, sort)) c1

-- | Select rows where the column `SelAttrName' contains True.
select :: Expr -> AlgNode -> GraphM a TableAlgebra AlgNode
select sel c1 = insertNode $ UnOp (Select sel) c1

-- | Remove duplicate rows
distinct :: AlgNode -> GraphM a TableAlgebra AlgNode
distinct c1 = insertNode $ UnOp (Distinct ()) c1

-- | Make cross product from two plans
cross :: AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
cross c1 c2 = insertNode $ BinOp (Cross ()) c1 c2

-- | Union between two plans
union :: AlgNode -> AlgNode -> GraphM a TableAlgebra AlgNode
union c1 c2 = insertNode $ BinOp (DisjUnion ()) c1 c2

-- | Project/rename certain column out of a plan
proj :: [Proj] -> AlgNode -> GraphM a TableAlgebra AlgNode
proj ps c = insertNode $ UnOp (Project ps) c

-- | Apply aggregate functions to a plan
aggr :: [(AggrType, ResAttrName)] -> [(AttrName, Expr)] -> AlgNode -> GraphM a TableAlgebra AlgNode
aggr aggrs part c1 = insertNode $ UnOp (Aggr (aggrs, part)) c1

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownum :: AttrName -> [AttrName] -> Maybe AttrName -> AlgNode -> GraphM a TableAlgebra AlgNode
rownum res sort part c1 = insertNode $ UnOp (RowNum (res, zip sort $ repeat Asc, part)) c1

-- | Same as rownum but columns can be assigned an ordering direction
rownum' :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> AlgNode -> GraphM a TableAlgebra AlgNode
rownum' res sort part c1 = insertNode $ UnOp (RowNum (res, sort, part)) c1

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
thetaJoinM :: SemInfJoin -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
thetaJoinM cond = bind2 (thetaJoin cond)

-- | Perform a semi join on two plans
semiJoinM :: SemInfJoin -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
semiJoinM cond = bind2 (semiJoin cond)

-- | Perform an anti join on two plans
antiJoinM :: SemInfJoin -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
antiJoinM cond = bind2 (antiJoin cond)

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoinM :: String -> String -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
eqJoinM n1 n2 = bind2 (eqJoin n1 n2)

-- | Assign a number to each row in column 'ResAttrName' incrementing
-- sorted by `sort'. The numbering is not dense!
rankM :: ResAttrName -> [SortAttr] -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
rankM res sort = bind1 (rank res sort)

-- | Compute the difference between two plans.
differenceM :: GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
differenceM = bind2 difference

-- | Same as rank but provides a dense numbering.
rowrankM :: ResAttrName -> [SortAttr] -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
rowrankM res sort = bind1 (rowrank res sort)

-- | Select rows where the column `SelAttrName' contains True.
selectM :: Expr -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
selectM sel = bind1 (select sel)

-- | Remove duplicate rows
distinctM :: GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
distinctM = bind1 distinct

-- | Make cross product from two plans
crossM :: GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
crossM = bind2 cross

-- | Union between two plans
unionM :: GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
unionM = bind2 union

-- | Project/rename certain column out of a plan
projM :: [Proj] -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
projM cols = bind1 (proj cols)

-- | Apply aggregate functions to a plan
aggrM :: [(AggrType, ResAttrName)] -> [(AttrName, Expr)] -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
aggrM aggrs part = bind1 (aggr aggrs part)

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownumM :: AttrName -> [AttrName] -> Maybe AttrName -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
rownumM res sort part = bind1 (rownum res sort part)

-- | Same as rownum but columns can be assigned an ordering direction
rownum'M :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> GraphM a TableAlgebra AlgNode -> GraphM a TableAlgebra AlgNode
rownum'M res sort part = bind1 (rownum' res sort part)
