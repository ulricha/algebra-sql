{-
This module contains helper function for constructing algebraic plans
-}
module Ferry.Algebra.Data.Create where
    
import Ferry.Algebra.Data.Algebra

-- | Create an algebraic int value
int :: Integer -> AVal
int = VInt

-- | Create an algebraic string value
string :: String -> AVal
string = VStr

-- | Create an algebraic boolean value
bool :: Bool -> AVal
bool = VBool

-- | Create an algebraic double value
double :: Double -> AVal
double = VDouble

-- | Create an algebraic decimal value
dec :: Float -> AVal
dec = VDec

-- | Create an algebraic nat value
nat :: Integer -> AVal
nat = VNat

-- | Types of algebraic values
intT, stringT, boolT, decT, doubleT, natT :: ATy
intT = AInt
stringT = AStr
boolT = ABool
decT = ADec
doubleT = ADouble
natT = ANat

-- | Construct an empty table node with 
emptyTable :: SchemaInfos -> AlgNode
emptyTable = (\x -> (x, [])) . EmptyTable

dbTable :: String -> Columns -> KeyInfos -> AlgNode
dbTable n cs ks = (TableRef (n, attr, ks), []) 
  where
    attr = map (\(NCol n [Col i t]) -> (n, "item" ++ show i, t)) cs

litTable :: AVal -> String -> ATy -> AlgNode
litTable v s t = (LitTable [[v]] [(s, t)], [])

attach :: ResAttrName -> ATy -> AVal -> Int -> AlgNode
attach n t v c = (Attach (n, (t, v)), [c])

eqJoin :: String -> String -> Int -> Int -> AlgNode
eqJoin n1 n2 c1 c2 = (EqJoin (n1, n2), [c1, c2])

rank :: ResAttrName -> SortInf -> Int -> AlgNode
rank res sort c1 = (Rank (res, sort), [c1])

cross :: Int -> Int -> AlgNode
cross c1 c2 = (Cross, [c1, c2])

union :: Int -> Int -> AlgNode
union c1 c2 = (DisjUnion, [c1, c2])

proj :: ProjInf -> Int -> AlgNode
proj cols c = (Proj cols, [c])

oper :: String -> ResAttrName -> LeftAttrName -> RightAttrName -> Int -> AlgNode
oper o r la ra c = (FunBinOp (o, r, la, ra), [c])