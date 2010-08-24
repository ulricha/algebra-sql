{-
This module contains helper function for constructing algebraic plans
-}
module Ferry.Algebra.Data.Create where
    
import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder

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
surT = ASur

-- | Graph construction combinators for table algebra

-- | Construct an empty table node with 
emptyTable :: SchemaInfos -> GraphM AlgNode
emptyTable = insertNode . (\x -> (x, [])) . EmptyTable

dbTable :: String -> Columns -> KeyInfos -> GraphM AlgNode
dbTable n cs ks = insertNode (TableRef (n, attr, ks), []) 
  where
    attr = map (\(NCol n [Col i t]) -> (n, "item" ++ show i, t)) cs

litTable :: AVal -> String -> ATy -> GraphM AlgNode
litTable v s t = insertNode (LitTable [[v]] [(s, t)], [])

attach :: ResAttrName -> ATy -> AVal -> AlgNode -> GraphM AlgNode
attach n t v c = insertNode (Attach (n, (t, v)), [c])

eqJoin :: String -> String -> AlgNode -> AlgNode -> GraphM AlgNode
eqJoin n1 n2 c1 c2 = insertNode (EqJoin (n1, n2), [c1, c2])

eqTJoin :: [(String, String)] -> ProjInf -> AlgNode -> AlgNode -> GraphM AlgNode
eqTJoin eqs projI q1 q2 = let (a, b) = head eqs
                          in foldr filterEqs (eqJoin a b q1 q2) $ tail eqs
        where resCol = "item99999002"
              filterEqs :: (String, String) -> GraphM AlgNode -> GraphM AlgNode
              filterEqs (l, r) res = proj projI =<< select resCol =<< oper "==" resCol l r =<< res

rank :: ResAttrName -> SortInf -> AlgNode -> GraphM AlgNode
rank res sort c1 = insertNode (Rank (res, sort), [c1])

rowrank :: ResAttrName -> SortInf -> AlgNode -> GraphM AlgNode
rowrank res sort c1 = insertNode (RowRank (res, sort), [c1])

posSelect :: Int -> SortInf -> Maybe AttrName -> AlgNode -> GraphM AlgNode
posSelect n sort part c1 = insertNode (PosSel (n, sort, part), [c1])

select :: SelAttrName -> AlgNode -> GraphM AlgNode
select sel c1 = insertNode (Sel sel, [c1])

distinct :: AlgNode -> GraphM AlgNode
distinct c1 = insertNode (Distinct, [c1])

cross :: AlgNode -> AlgNode -> GraphM AlgNode
cross c1 c2 = insertNode (Cross, [c1, c2])

notC :: AttrName -> AttrName -> AlgNode -> GraphM AlgNode
notC r n c1 = insertNode (FunBoolNot (r, n), [c1])

union :: AlgNode -> AlgNode -> GraphM AlgNode
union c1 c2 = insertNode (DisjUnion, [c1, c2])

proj :: ProjInf -> AlgNode -> GraphM AlgNode
proj cols c = insertNode (Proj cols, [c])

aggr :: [(AggrType, ResAttrName, AttrName)] -> Maybe PartAttrName -> AlgNode -> GraphM AlgNode
aggr aggrs part c1 = insertNode (Aggr (aggrs, part), [c1])

rownum :: AttrName -> [AttrName] -> Maybe AttrName -> AlgNode -> GraphM AlgNode
rownum res sort part c1 = insertNode (RowNum (res, zip sort $ repeat Asc, part) , [c1])

oper :: String -> ResAttrName -> LeftAttrName -> RightAttrName -> AlgNode -> GraphM AlgNode
oper o r la ra c = insertNode (FunBinOp (o, r, la, ra), [c])

initLoop :: AlgConstr
initLoop =  (LitTable [[(nat 1)]] [("iter", natT)], [])   