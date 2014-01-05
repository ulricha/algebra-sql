{-|
This module contains helper functions for constructing algebraic plans.
-}

module Database.Algebra.Pathfinder.Data.Create where

import           Database.Algebra.Dag.Builder
import           Database.Algebra.Dag.Common
import           Database.Algebra.Pathfinder.Data.Algebra

-- * Value constructors

-- | Create an PFAlgebraic int value
int :: Integer -> AVal
int = VInt

-- | Create an PFAlgebraic string value
string :: String -> AVal
string = VStr

-- | Create an PFAlgebraic boolean value
bool :: Bool -> AVal
bool = VBool

-- | Create an PFAlgebraic double value
double :: Double -> AVal
double = VDouble

-- | Create an PFAlgebraic decimal value
dec :: Float -> AVal
dec = VDec

-- | Create an PFAlgebraic nat value
nat :: Integer -> AVal
nat = VNat

-- | Types of PFAlgebraic values
intT, stringT, boolT, decT, doubleT, natT :: ATy
intT = AInt
stringT = AStr
boolT = ABool
decT = ADec
doubleT = ADouble
natT = ANat

-- * Graph construction combinators for table PFAlgebra

-- | Construct an empty table node with
emptyTable :: SchemaInfos -> GraphM a PFAlgebra AlgNode
emptyTable schema = insertNode $ NullaryOp (EmptyTable schema)

-- | Construct a database table node
-- The first argument is the \emph{qualified} name of the database
-- table. The second describes the columns in alphabetical order.
-- The third argument describes the database keys (one table key can
-- span over multiple columns).
dbTable :: String -> [(AttrName, ATy)] -> [Key] -> GraphM a PFAlgebra AlgNode
dbTable n cs ks = insertNode $ NullaryOp $ TableRef (n, cs, ks)

-- | Construct a table with one value
litTable :: AVal -> String -> ATy -> GraphM a PFAlgebra AlgNode
litTable v s t = insertNode $ NullaryOp $ LitTable [[v]] [(s, t)]

-- | Construct a literal table with multiple columns and rows
litTable' :: [[AVal]] -> [(String, ATy)] -> GraphM a PFAlgebra AlgNode
litTable' v s = insertNode $ NullaryOp $ LitTable v s

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoin :: LeftAttrName -> RightAttrName -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
eqJoin n1 n2 c1 c2 = insertNode $ BinOp (EqJoin (n1, n2)) c1 c2

thetaJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
thetaJoin cond c1 c2 = insertNode $ BinOp (ThetaJoin cond) c1 c2

semiJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
semiJoin cond c1 c2 = insertNode $ BinOp (SemiJoin cond) c1 c2

antiJoin :: SemInfJoin -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
antiJoin cond c1 c2 = insertNode $ BinOp (AntiJoin cond) c1 c2

-- | Assign a number to each row in column 'ResAttrName' incrementing
-- sorted by `SortInf'. The numbering is not dense!
rank :: ResAttrName -> SortInf -> AlgNode -> GraphM a PFAlgebra AlgNode
rank res sort c1 = insertNode $ UnOp (Rank (res, sort)) c1

-- | Compute the difference between two plans.
difference :: AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
difference q1 q2 = insertNode $ BinOp (Difference ()) q1 q2

-- | Same as rank but provides a dense numbering.
rowrank :: ResAttrName -> SortInf -> AlgNode -> GraphM a PFAlgebra AlgNode
rowrank res sort c1 = insertNode $ UnOp (RowRank (res, sort)) c1

-- | Get's the nth element(s) of a (partitioned) table.
posSelect :: Int -> SortInf -> Maybe AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
posSelect n sort part c1 = insertNode $ UnOp (PosSel (n, sort, part)) c1

-- | Select rows where the column `SelAttrName' contains True.
select :: Expr -> AlgNode -> GraphM a PFAlgebra AlgNode
select sel c1 = insertNode $ UnOp (Select sel) c1

-- | Remove duplicate rows
distinct :: AlgNode -> GraphM a PFAlgebra AlgNode
distinct c1 = insertNode $ UnOp (Distinct ()) c1

-- | Make cross product from two plans
cross :: AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
cross c1 c2 = insertNode $ BinOp (Cross ()) c1 c2

-- | Union between two plans
union :: AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
union c1 c2 = insertNode $ BinOp (DisjUnion ()) c1 c2

-- | Project/rename certain column out of a plan
proj :: [Proj] -> AlgNode -> GraphM a PFAlgebra AlgNode
proj ps c = insertNode $ UnOp (Project ps) c

-- | Apply aggregate functions to a plan
aggr :: [(AggrType, ResAttrName)] -> [PartAttrName] -> AlgNode -> GraphM a PFAlgebra AlgNode
aggr aggrs part c1 = insertNode $ UnOp (Aggr (aggrs, part)) c1

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownum :: AttrName -> [AttrName] -> Maybe AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
rownum res sort part c1 = insertNode $ UnOp (RowNum (res, zip sort $ repeat Asc, part)) c1

-- | Same as rownum but columns can be assigned an ordering direction
rownum' :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
rownum' res sort part c1 = insertNode $ UnOp (RowNum (res, sort, part)) c1

initLoop :: PFAlgebra
initLoop = NullaryOp $ LitTable [[nat 1]] [("iter", natT)]
