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
intT, stringT, boolT, decT, doubleT, natT, surT :: ATy
intT = AInt
stringT = AStr
boolT = ABool
decT = ADec
doubleT = ADouble
natT = ANat
surT = ASur

-- * Graph construction combinators for table PFAlgebra

-- | Construct an empty table node with
emptyTable :: SchemaInfos -> GraphM a PFAlgebra AlgNode
emptyTable schema = insertNode $ NullaryOp (EmptyTable schema)

-- | Construct a database table node
-- The first argument is the \emph{qualified} name of the database
-- table. The second describes the columns in alphabetical order.
-- The third argument describes the database keys (one table key can
-- span over multiple columns).
dbTable :: String -> Columns -> KeyInfos -> GraphM a PFAlgebra AlgNode
dbTable n cs ks = insertNode $ NullaryOp $ TableRef (n, attr, ks)
  where
    attr = map (\c -> case c of
                        (NCol n' [Col i t]) -> (n', "item" ++ show i, t)
                        _                   -> error "Not a named column") cs

-- | Construct a table with one value
litTable :: AVal -> String -> ATy -> GraphM a PFAlgebra AlgNode
litTable v s t = insertNode $ NullaryOp $ LitTable [[v]] [(s, t)]

-- | Construct a literal table with multiple columns and rows
litTable' :: [[AVal]] -> [(String, ATy)] -> GraphM a PFAlgebra AlgNode
litTable' v s = insertNode $ NullaryOp $ LitTable v s

-- | Attach a column 'ResAttrName' of type `ATy' with value
-- `AVal' in all rows to table `AlgNode'
attach :: ResAttrName -> ATy -> AVal -> AlgNode -> GraphM a PFAlgebra AlgNode
attach n t v c = insertNode $ UnOp (Attach (n, (t, v))) c

-- | Cast column `AttrName' to type `ATy' and give it the name
--  `ResAttrName' afterwards.
cast :: AttrName -> ResAttrName -> ATy -> AlgNode -> GraphM a PFAlgebra AlgNode
cast n r t c = insertNode $ UnOp (Cast (r, n, t)) c

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoin :: String -> String -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
eqJoin n1 n2 c1 c2 = insertNode $ BinOp (EqJoin (n1, n2)) c1 c2

-- | The same as eqJoin but with multiple columns.
eqTJoin :: [(String, String)] -> ProjInf -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
eqTJoin eqs projI q1 q2 = let (a, b) = head eqs
                          in foldr filterEqs (eqJoin a b q1 q2) $ tail eqs
        where resCol = "item99999002"
              filterEqs :: (String, String) -> GraphM a PFAlgebra AlgNode -> GraphM a PFAlgebra AlgNode
              filterEqs (l, r) res = proj projI =<< select resCol =<< oper "==" resCol l r =<< res

thetaJoin :: LeftAttrName -> String -> RightAttrName -> AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
thetaJoin n1 o n2 c1 c2 = insertNode $ BinOp (ThetaJoin (n1, n2, o)) c1 c2

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
select :: SelAttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
select sel c1 = insertNode $ UnOp (Sel sel) c1

-- | Remove duplicate rows
distinct :: AlgNode -> GraphM a PFAlgebra AlgNode
distinct c1 = insertNode $ UnOp (Distinct ()) c1

-- | Make cross product from two plans
cross :: AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
cross c1 c2 = insertNode $ BinOp (Cross ()) c1 c2

-- | Negate the boolen value in column n and store it in column r
notC :: AttrName -> AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
notC r n c1 = insertNode $ UnOp (FunBoolNot (r, n)) c1

-- | Union between two plans
union :: AlgNode -> AlgNode -> GraphM a PFAlgebra AlgNode
union c1 c2 = insertNode $ BinOp (DisjUnion ()) c1 c2

-- | Project/rename certain column out of a plan
proj :: ProjInf -> AlgNode -> GraphM a PFAlgebra AlgNode
proj cols c = insertNode $ UnOp (Proj cols) c

-- | Apply aggregate functions to a plan
aggr :: [(AggrType, ResAttrName, Maybe AttrName)] -> Maybe PartAttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
aggr aggrs part c1 = insertNode $ UnOp (Aggr (aggrs, part)) c1

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownum :: AttrName -> [AttrName] -> Maybe AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
rownum res sort part c1 = insertNode $ UnOp (RowNum (res, zip sort $ repeat Asc, part)) c1

-- | Same as rownum but columns can be assigned an ordering direction
rownum' :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
rownum' res sort part c1 = insertNode $ UnOp (RowNum (res, sort, part)) c1

-- | Apply an operator to the element in `LeftAttrName' and `RightAttrName',
-- store the result in `ResAttrName'
oper :: String -> ResAttrName -> LeftAttrName -> RightAttrName -> AlgNode -> GraphM a PFAlgebra AlgNode
oper o r la ra c = insertNode $ UnOp (FunBinOp (o, r, la, ra)) c


initLoop :: PFAlgebra
initLoop = NullaryOp $ LitTable [[nat 1]] [("iter", natT)]
