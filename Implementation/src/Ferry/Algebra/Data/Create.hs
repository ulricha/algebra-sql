module Ferry.Algebra.Data.Create where
    
import Ferry.Algebra.Data.Algebra

int :: Integer -> AVal
int = VInt

string :: String -> AVal
string = VStr

bool :: Bool -> AVal
bool = VBool

double :: Double -> AVal
double = VDouble

dec :: Float -> AVal
dec = VDec

nat :: Integer -> AVal
nat = VNat

intT, stringT, boolT, decT, doubleT, natT :: ATy
intT = AInt
stringT = AStr
boolT = ABool
decT = ADec
doubleT = ADouble
natT = ANat

litTable :: AVal -> String -> ATy -> AlgNode
litTable v s t = (LitTable [[v]] [(s, t)], [])

attach :: ResAttrName -> ATy -> AVal -> Int -> AlgNode
attach n t v c = (Attach (n, (t, v)), [c])

eqJoin :: String -> String -> Int -> Int -> AlgNode
eqJoin n1 n2 c1 c2 = (EqJoin (n1, n2), [c1, c2])

proj :: ProjInf -> Int -> AlgNode
proj cols c = (Proj cols, [c])