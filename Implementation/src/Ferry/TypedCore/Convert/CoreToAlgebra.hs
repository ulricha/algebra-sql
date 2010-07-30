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

import Ferry.TypedCore.Data.Type (Qual (..), FType (..), RLabel (..))
import Ferry.TypedCore.Data.TypedCore as T

import qualified Data.Map as M 

-- | Section introducing aliases for commonly used columns

-- | Results are stored in column:
resCol = "item99999001"
ordCol = "item99999801"
iterPrime = "item99999701"
posPrime = "item99999601"
outer = "item99999501"
inner = "item99999401"

-- | Construct the ith item columns
mkPrefixCol i = "item" ++ prefixCol ++ (show i)

-- | Construct the ith iter column
mkPrefixIter i = "iter" ++ prefixCol ++ (show i)

-- | Prefix for intermediate column numbers
prefixCol = "9999"

-- | Transform Ferry core into a relation algebra modelled as a DAG
coreToAlgebra :: CoreExpr -> GraphM AlgRes
-- | Primitive values
coreToAlgebra (Constant t (CInt i)) = do 
                                        n1 <- attach "pos" intT (int 1) 
                                                =<< attach "item1" intT (int i) 
                                                =<< getLoop
                                        return (n1, [Col 1 AInt], EmptySub)
coreToAlgebra (Constant t (CBool i)) = do
                                         n1 <- attach "pos" intT (int 1) 
                                                =<< attach "item1" boolT (bool i) 
                                                =<< getLoop
                                         return (n1, [Col 1 ABool], EmptySub)
coreToAlgebra (Constant t (CFloat i)) = do
                                         n1 <- attach "pos" intT (int 1) 
                                            =<< attach "item1" doubleT (double i) 
                                            =<< getLoop
                                         return (n1, [Col 1 ADouble], EmptySub)
coreToAlgebra (Constant t (CString i)) = do
                                          n1 <- attach "pos" intT (int 1) 
                                                =<< attach "item1" stringT (string i) 
                                                =<< getLoop
                                          return (n1, [Col 1 AStr], EmptySub)
-- Binary operators
coreToAlgebra (BinOp (_ :=> t) (Op o) e1 e2) = do
                                         (q1, [Col 1 t1], m1) <- coreToAlgebra e1
                                         (q2, [Col 1 t2], m2) <- coreToAlgebra e2
                                         n1 <- proj [("iter", "iter"), ("pos", "pos"), ("item1", resCol)] 
                                                =<< oper o resCol  "item1" (mkPrefixCol 1) 
                                                =<< eqJoin "iter" (mkPrefixIter 1) q1 
                                                =<< proj [(mkPrefixIter 1, "iter"), (mkPrefixCol 1, "item1")] q2
                                         return (n1, fst $ typeToCols t 1, EmptySub)
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
                                n1 <- proj (("iter", "iter"):("pos", "pos"):projPairs) q1
                                return (n1, csn', EmptySub)
--Empty lists
coreToAlgebra (Nil (_ :=> t)) = do
                                 let cs = fst $ typeToCols t 1
                                 let schema = ("iter", AInt):("pos", AInt):(colsToSchema cs)
                                 n1 <- emptyTable schema
                                 return (n1, cs, EmptySub)
-- List constructor, because of optimisation chances contents has been directed to special functions
coreToAlgebra (c@(Cons _ _ _)) = listFirst c
-- Database tables
coreToAlgebra (Table t n cs ks) = do
                                    let cs' = coreCol2AlgCol cs
                                    let keys = key2Key cs' ks
                                    loop <- getLoop
                                    n1 <- cross loop 
                                            =<< rank "pos" (map (\ki -> (ki, Asc)) $ head keys) 
                                            =<< dbTable n cs' keys
                                    return (n1, cs', EmptySub)
-- If then else
coreToAlgebra (If t e1 e2 e3) = do
                                  (q1, cs1, ts1) <- coreToAlgebra e1
                                  -- Get current gamma
                                  gam <- getGamma
                                  -- Build loop and gamma for then branch 
                                  loopThen <- proj [("iter", "iter")] =<< select "item1" q1
                                  gamThen <- transformGam algResLoop loopThen gam 
                                  --Evaluate then branch
                                  (q2, cs2, ts2) <- withContext gamThen loopThen $ coreToAlgebra e2
                                  -- Build loop and gamma for else branch
                                  loopElse <- proj [("iter", "iter")] 
                                                =<< select resCol 
                                                =<< notC resCol "item1" q1
                                  gamElse <- transformGam algResLoop loopElse gam 
                                  --Evaluate else branch
                                  (q3, cs3, ts3) <- withContext gamElse loopElse $ coreToAlgebra e3
                                  --Construct result
                                  let projPairs = zip (leafNames cs2) (leafNames cs2)
                                  n1 <- attach ordCol intT (int 1) q2
                                  n2 <- proj (("iter","iter"):("pos","pos"):projPairs) 
                                            =<< union n1 
                                            =<< attach ordCol intT (int 2) q3
                                  return (n2, cs2, EmptySub)
-- Compile function application, as we do not have functions as results the given
-- argument can be evaluated and then be passed to the compileApp function.
coreToAlgebra (App t e1 e2) = compileAppE1 e1 =<< compileParam e2
                                

-- | Transform the variable environment                                  
transformGam :: (AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)) 
                -> AlgNode -> Gam -> GraphM Gam
transformGam f loop gamma  = mapM (f loop) gamma

-- | Transformation of gamma for if then else    
algResLoop :: AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)
algResLoop loop (n, (i, cs, pl)) = do
                              i' <- eqJoin "iter" "iter" i loop
                              return (n, (i', cs, pl))

-- | Compile a function parameter
-- | Function is partial, i.e. it doesn't compile lambda's as arguments                              
compileParam :: Param -> GraphM AlgRes
compileParam (ParExpr t e1) = coreToAlgebra e1

-- | Compile function application.
-- | Expects a core expression the function, and the evaluated argument
compileAppE1 :: CoreExpr -> AlgRes -> GraphM AlgRes
compileAppE1 (App t (Var mt "map") l@(ParAbstr _ _ _)) (q1, cs1, ts1) = 
                do
                    let csProj = zip (leafNames cs1) (leafNames cs1)
                    qv' <- rownum inner ["iter", "pos"] Nothing q1
                    qv <-  proj (("iter", inner):("pos", posPrime):csProj)
                                =<< attach posPrime intT (int 1) qv'
                    loopv <- proj [("iter",inner)] qv'
                    mapv <- proj [(outer, "iter"), (inner, inner), (posPrime, "pos")] qv'
                    gamV <- transformGam algResv mapv =<< getGamma
                    (q2, cs2, ts2) <- withContext gamV loopv $ compileLambda (qv, cs1, ts1) l
                    let csProj2 = zip (leafNames cs2) (leafNames cs2)
                    q <- proj (("iter",outer):("pos", posPrime):csProj2)
                            =<< eqJoin "iter" inner q2 mapv
                    return (q, cs2, EmptySub)

-- | Compile a lambda where the argument variable is bound to the given expression                    
compileLambda :: AlgRes -> Param -> GraphM AlgRes
compileLambda arg (ParAbstr t (PVar x) e) = withBinding x arg $ coreToAlgebra e

-- | Transform gamma for map function                
algResv :: AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)
algResv m (n, (q, cs, ts)) = do
                                let projPairs = zip (leafNames cs) (leafNames cs)
                                q' <- proj (("iter", inner):("pos","pos"):projPairs) =<< eqJoin "iter" outer q m
                                return (n, (q', cs, ts))


-- Compilation for the first element of a list.
-- For optimisation purposes we distinguish three cases:
-- Singleton lists: compile these if they were just single values
-- A list where the second element is also created through a list constructor
--      that particular case allows for optimising on the rank operator, it is
--      compiled to algebra in the listSequence function that does not perform rank.
-- A list where the tail is the result of a computation, the tail is compiled as a
--      normal expression. The result get an ord column attached and the is unified
--      with the head of the list and then ranked.    
listFirst :: CoreExpr -> GraphM AlgRes
listFirst (Cons t e1 (Nil _)) = coreToAlgebra e1
listFirst (Cons t e1 e2@(Cons _ _ _)) = do
                                         (q1, cs1, ts1) <- coreToAlgebra e1
                                         (q2, cs2, ts2) <- listSequence e2 2
                                         let projPairs = zip (leafNames cs1) (leafNames cs1)
                                         n1 <- proj (("iter", "iter"):("pos", resCol):projPairs) 
                                                =<< rank resCol [(ordCol, Asc), ("pos", Asc)] 
                                                    =<< flip union q2 
                                                        =<< attach ordCol intT (int 1) q1
                                         return (n1, cs1, EmptySub)
listFirst (Cons t e1 e2) = do
                            (q1, cs1, ts1) <- coreToAlgebra e1
                            (q2, cs2, ts2) <- coreToAlgebra e2
                            n1 <- attach ordCol intT (int 1) q1
                            let projPairs = zip (leafNames cs1) (leafNames cs1)
                            n2 <- proj (("iter", "iter"):("pos", resCol):projPairs) 
                                    =<< rank resCol [(ordCol, Asc), ("pos", Asc)]
                                        =<< union n1 
                                            =<< attach ordCol intT (int 2) q2
                            return (n2, cs1, EmptySub)

-- List sequence, doesn't perform the rank operation, that is carried out by listFirst.
--  Three cases with similar motivation as listFirst.
listSequence :: CoreExpr -> Int -> GraphM AlgRes
listSequence (Cons t e1 (Nil _)) n = do
                                      (q1, cs1, ts1) <- coreToAlgebra e1
                                      n1 <- attach ordCol intT (int $ toEnum n) q1
                                      return (n1, cs1, EmptySub)
listSequence (Cons t e1 e2@(Cons _ _ _)) n = do
                                                (q1, cs1, ts1) <- coreToAlgebra e1
                                                (q2, cs2, ts2) <- listSequence e2 $ n + 1
                                                n1 <- attach ordCol intT (int $ toEnum n) q1
                                                n2 <- union n1 q2
                                                return (n2, cs1, EmptySub)
listSequence (Cons t e1 e2) n = do
                                 (q1, cs1, ts1) <- coreToAlgebra e1
                                 (q2, cs2, ts2) <- coreToAlgebra e2
                                 n1 <- attach ordCol intT (int $ toEnum n) q1
                                 n2 <- attach ordCol intT (int $ toEnum (n + 1)) q2
                                 n3 <- union n1 n2
                                 return (n3, cs1, EmptySub)
                                    
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
                                n1 <- proj ((mkPrefixIter 1, "iter"):projPairs) q2
                                n2 <- eqJoin "iter" (mkPrefixIter 1) q1 n1
                                let projPairs' = zip (leafNames cs1) (leafNames cs1) ++ zip (leafNames cs2') (leafNames cs2')
                                n3 <- proj (("iter", "iter"):("pos", "pos"):projPairs') n2
                                return (n3, cs1 ++ cs2', EmptySub)

-- Function to transform the column structure

--From a typedcore column list to algebraic columns
coreCol2AlgCol :: [T.Column] -> Columns
coreCol2AlgCol cols = map (\(Column s t, i) -> NCol s $ fst $ typeToCols t i) cols'
    where
      cols' = zip cols [1..]

--Translate core keys to algebraic keys
key2Key :: Columns -> [Key] -> KeyInfos
key2Key cs ks = map (\(Key k) -> map (\ki -> case getCol ki cs of
                                        [(Col i _)] -> "item" ++ show i) k ) ks

-- Get all the column names from the structure                                    
leafNames :: Columns -> [String]
leafNames cs = map (\(Col i _) -> "item" ++ show i) $ colLeafs cs

-- Get all the leaf columns, that is the columns that are actually a column
colLeafs :: Columns -> Columns
colLeafs (c@(Col i _):xs) = (:) c $ colLeafs xs
colLeafs ((NCol _ cs):xs) = colLeafs cs ++ colLeafs xs
colLeafs []               = []

-- Count the number of columns
colSize :: Columns -> Int
colSize = length . colLeafs

-- Increment the column numbers by a given amount
incrCols :: Int -> Columns -> Columns
incrCols inc ((Col i t):xs)    = (Col (i + inc) t):(incrCols inc xs)
incrCols inc ((NCol x i):xs) = (NCol x (incrCols inc i)):(incrCols inc xs)
incrCols inc []              = [] 

-- Find the lowest column number
minCol :: Columns -> Int
minCol c = minimum $ (:) 1 $ map (\(Col i _) -> i) $ colLeafs c

-- Decrement the column numbers so that the lowest column number is 1 after applying
decrCols :: Columns -> Columns
decrCols cols = let minV = minCol cols
                 in decr' (minV - 1) cols
    where
     decr' :: Int -> Columns -> Columns
     decr' decr ((Col i t):xs)    = (flip Col t $ i - decr) : (decr' decr xs)
     decr' decr ((NCol x i):xs) = (NCol x $ decr' decr i) : (decr' decr xs)
     decr' _    []              = []

-- Find the columns associated with a record label
getCol :: String -> Columns -> Columns
getCol n cs = getCol' cs
    where
     getCol' :: Columns -> Columns
     getCol' ((Col i _):xs)              = getCol' xs
     getCol' ((NCol x i):xs) | x == n    = i
                             | otherwise = getCol' xs
     getCol' []                          = []

-- Transform Columns info into schema info for algebraic compilation
colsToSchema :: Columns -> SchemaInfos
colsToSchema ((Col i t):xs) = (:)("item" ++ show i, t) $ colsToSchema xs
colsToSchema ((NCol _ cs):xs) = colsToSchema cs ++ colsToSchema xs
colsToSchema [] = []

-- Transform a type to columns structure
typeToCols :: FType -> Int -> (Columns, Int)
typeToCols (FRec recs) i = recsToCols recs i
typeToCols FInt i = ([Col i AInt], i + 1)
typeToCols FBool i = ([Col i ABool], i + 1)
typeToCols FFloat i = ([Col i ADouble], i + 1)
typeToCols FString i = ([Col i AStr], i + 1)
typeToCols (FList _) i = ([Col i ASur], i + 1)

-- Compile a record type to a column structure
recsToCols :: [(RLabel, FType)] -> Int -> (Columns, Int)
recsToCols ((RLabel s, ty):xs) i = let (cs, i') = typeToCols ty i
                                       (cs', i'') = recsToCols xs i'
                                    in ((NCol s cs):cs',  i'')
recsToCols [] i = ([], i)