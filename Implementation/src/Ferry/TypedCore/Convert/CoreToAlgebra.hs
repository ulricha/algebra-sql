module Ferry.TypedCore.Convert.CoreToAlgebra where

{-
This module transforms typed ferry core into a relational algebra DAG.
The transformation assumes that given programs are type correct and some
functions on lists have been inlined (transformations performed by RewriteStage).
-}
import Ferry.Front.Data.Base
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.Create
import Ferry.Algebra.Data.GraphBuilder

import Ferry.TypedCore.Data.TypedCore

import qualified Data.Map as M 


--Results are stored in column:
resCol = "item999999001"

--Construct the ith item columns
mkPrefixCol i = "item" ++ prefixCol ++ (show i)

--Construct the ith iter column
mkPrefixIter i = "iter" ++ prefixCol ++ (show i)

--Prefix for intermediate column numbers
prefixCol = "9999"

-- Transform Ferry core into a relation algebra modelled as a DAG
coreToAlgebra :: CoreExpr -> GraphM AlgRes
-- Primitive values
coreToAlgebra (Constant t (CInt i)) = do 
                                        loop <- getLoop
                                        n1 <- insertNode loop
                                        n2 <- insertNode $ attach "item1" intT (int i) n1
                                        n3 <- insertNode $ attach "pos" intT (int 1) n2
                                        return (n3, [Col 1], EmptySub)
coreToAlgebra (Constant t (CBool i)) = do
                                         loop <- getLoop
                                         n1 <- insertNode loop
                                         n2 <- insertNode $ attach "item1" boolT (bool i) n1
                                         n3 <- insertNode $ attach "pos" intT (int 1) n2
                                         return (n3, [Col 1], EmptySub)
coreToAlgebra (Constant t (CFloat i)) = do
                                         loop <- getLoop
                                         n1 <- insertNode loop
                                         n2 <- insertNode $ attach "item1" doubleT (double i) n1
                                         n3 <- insertNode $ attach "pos" intT (int 1) n2
                                         return (n3, [Col 1], EmptySub)
coreToAlgebra (Constant t (CString i)) = do
                                          loop <- getLoop
                                          n1 <- insertNode loop
                                          n2 <- insertNode $ attach "item1" stringT (string i) n1
                                          n3 <- insertNode $ attach "pos" intT (int 1) n2
                                          return (n3, [Col 1], EmptySub)
-- Binary operators
coreToAlgebra (BinOp t (Op o) e1 e2) = do
                                         (q1, [Col 1], m1) <- coreToAlgebra e1
                                         (q2, [Col 1], m2) <- coreToAlgebra e2
                                         n1 <- insertNode $ proj [(mkPrefixIter 1, "iter"), (mkPrefixCol 1, "item1")] q2
                                         n2 <- insertNode $ eqJoin "iter" (mkPrefixIter 1) q1 n1
                                         n3 <- insertNode $ oper o resCol  "item1" (mkPrefixCol 1) n2
                                         n4 <- insertNode $ proj [("iter", "iter"), ("pos", "pos"), ("item1", resCol)] n3
                                         return (n4, [Col 1], EmptySub)
-- Let bindings
coreToAlgebra (Let t s e1 e2) = do
                                    (q1, cs1, m1) <- coreToAlgebra e1
                                    withBinding s (q1, cs1, m1) $ coreToAlgebra e2
-- Variable lookup
coreToAlgebra (Var t n) = fromGam n
-- Record construction, body of the rule can be found in recElemsToAlgebra
coreToAlgebra (Rec t (e:els)) = foldl recElemsToAlgebra (recElemToAlgebra e) els
-- Record element access.
coreToAlgebra (Elem t e n) = do
                                (q1, cs1 ,ts1) <- coreToAlgebra e
                                let csn = getCol n cs1
                                let csn' = decrCols csn
                                let projPairs = zip (leafNames csn') (leafNames csn)
                                n1 <- insertNode $ proj (("iter", "iter"):("pos", "pos"):projPairs) q1
                                return (n1, csn', EmptySub)

-- Transform a record element into algebraic plan                             
recElemToAlgebra :: RecElem -> GraphM AlgRes
recElemToAlgebra (RecElem t n e) = do
                                     (q1, cs1, ts1) <- coreToAlgebra e
                                     return (q1, [NCol n cs1], ts1)

-- Transform a record into an algebraic plan                                     
recElemsToAlgebra :: GraphM AlgRes -> RecElem -> GraphM AlgRes
recElemsToAlgebra alg2 el = do
                                (q1, cs1, ts1) <- alg2
                                (q2, cs2, ts2) <- recElemToAlgebra el
                                let offSet = colSize cs1
                                let cs2' = incrCols offSet cs2
                                let projPairs = zip (leafNames cs2') (leafNames cs2)
                                n1 <- insertNode $ proj ((mkPrefixIter 1, "iter"):projPairs) q2
                                n2 <- insertNode $ eqJoin "iter" (mkPrefixIter 1) q1 n1
                                let projPairs' = zip (leafNames cs1) (leafNames cs1) ++ zip (leafNames cs2') (leafNames cs2')
                                n3 <- insertNode $ proj (("iter", "iter"):("pos", "pos"):projPairs') n2
                                return (n3, cs1 ++ cs2', EmptySub)

-- Function to transform the column structure

-- Get all the column names from the structure                                    
leafNames :: Columns -> [String]
leafNames cs = map (\i -> "item" ++ show i) $ colLeafs cs

-- Get all the leaf columns, that is the columns that are actually a column
colLeafs :: Columns -> [Int]
colLeafs ((Col i):xs) = (:) i $ colLeafs xs
colLeafs ((NCol _ cs):xs) = colLeafs cs ++ colLeafs xs
colLeafs []               = []

-- Count the number of columns
colSize :: Columns -> Int
colSize = length . colLeafs

-- Increment the column numbers by a given amount
incrCols :: Int -> Columns -> Columns
incrCols inc ((Col i):xs)    = (Col (i + inc)):(incrCols inc xs)
incrCols inc ((NCol x i):xs) = (NCol x (incrCols inc i)):(incrCols inc xs)
incrCols inc []              = [] 

-- Find the lowest column number
minCol :: Columns -> Int
minCol c = minimum $ (:) 1 $ colLeafs c

-- Decrement the column numbers so that the lowest column number is 1 after applying
decrCols :: Columns -> Columns
decrCols cols = let minV = minCol cols
                 in decr' (minV - 1) cols
    where
     decr' :: Int -> Columns -> Columns
     decr' decr ((Col i):xs)    = (Col $ i - decr) : (decr' decr xs)
     decr' decr ((NCol x i):xs) = (NCol x $ decr' decr i) : (decr' decr xs)
     decr' _    []              = []

-- Find the columns associated with a record label
getCol :: String -> Columns -> Columns
getCol n cs = getCol' cs
    where
     getCol' :: Columns -> Columns
     getCol' ((Col i):xs)                = getCol' xs
     getCol' ((NCol x i):xs) | x == n    = i
                             | otherwise = getCol' xs
     getCol' []                          = []
{-
data CoreExpr where
    BinOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr -> CoreExpr
    UnaOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr
    Constant :: (Qual FType) -> Const -> CoreExpr
    Var  :: (Qual FType) -> String -> CoreExpr
    App :: (Qual FType) -> CoreExpr -> Param -> CoreExpr
    Let :: (Qual FType) -> String -> CoreExpr -> CoreExpr -> CoreExpr
    Rec :: (Qual FType) -> [RecElem] -> CoreExpr
    Cons :: (Qual FType) -> CoreExpr -> CoreExpr -> CoreExpr
    Nil :: (Qual FType) -> CoreExpr
    Elem :: (Qual FType) -> CoreExpr -> String -> CoreExpr
    Table :: (Qual FType) -> String -> [Column] -> [Key] -> CoreExpr
    If :: (Qual FType) -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
-}