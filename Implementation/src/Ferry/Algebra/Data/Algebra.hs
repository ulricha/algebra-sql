{-# LANGUAGE GADTs #-}
module Ferry.Algebra.Data.Algebra where

import Numeric (showFFloat)

--data SortDir = Asc
--             | Desc
--
--data JoinCompKind = TJ_EQ 
--                  | TJ_GT 
--                  | TJ_GE 
--                  | TJ_LT 
--                  | TJ_LE 
--                  | TJ_NE
--
data ATy where
    AInt :: ATy             
    AStr :: ATy             
    ABool :: ATy            
    ADec :: ATy             
    ADouble :: ATy          
    ANat :: ATy             
      deriving (Eq, Ord)
      

instance Show ATy where
  show AInt     = "int"
  show AStr     = "str"
  show ABool    = "bool"
  show ADec     = "dec"
  show ADouble  = "dbl"
  show ANat     = "nat"
                  
data AVal where
  VInt :: Integer -> AVal
  VStr :: String -> AVal
  VBool :: Bool -> AVal
  VDouble :: Double -> AVal
  VDec :: Float -> AVal 
  VNat :: Integer -> AVal
    deriving (Eq, Ord)

instance Show AVal where
  show (VInt x)     = show x
  show (VStr x)     = show x
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VDouble x)     =  show x
  show (VDec x)     = showFFloat (Just 2) x ""
  show (VNat x)     = show x

--data FunTy1To1 = FT1To1Add
--               | FT1To1Sub
--               | FT1To1Mul
--               | FT1To1Div
--               | FT1To1Mod
--               | FT1To1Con
--              deriving (Eq,  Show)
--
--data FunTyAggr = FTAggr_Avg
--               | FTAggr_Max
--               | FTAggr_Min
--               | FTAggr_Sum
--            deriving (Eq, Show)

type ATyVal = (ATy, AVal)

type AttrName            = String              
type ResAttrName         = AttrName
--type SortAttrName        = AttrName
--type PartAttrName        = AttrName
type NewAttrName         = AttrName
type OldAttrName         = AttrName
--type SelAttrName         = AttrName
type LeftAttrName        = AttrName
type RightAttrName       = AttrName
--
--type TableName           = String  
--type TableAttrInf        = [(AttrName, AttrName, ATy)]
--type KeyInfo             = [AttrName]
--type KeyInfos            = [KeyInfo]
--
--type SortInf              = [(SortAttrName, SortDir)]
type ProjInf              = [(NewAttrName, OldAttrName)]  

--type JoinPred = (JoinCompKind, (LeftAttrName,RightAttrName))
--type JoinPreds  = [JoinPred]

type Tuple = [AVal]

type SchemaInfos = [(AttrName, ATy)]    

-- type SemInfRowNum  = (ResAttrName, SortInf, Maybe PartAttrName) 
-- type SemInfRowId   = ResAttrName
-- type SemInfRank    = (ResAttrName,  SortInf)
type SemInfProj    = ProjInf
-- type SemInfSel     = SelAttrName
-- type SemInfPosSel  = (Int, SortInf, Maybe PartAttrName) 
type SemInfEqJoin  = (LeftAttrName,RightAttrName)
-- type SemInfThetaJoin = JoinPreds 
type SemInfLitTable = [Tuple]
-- type SemInfTableRef = (TableName, TableAttrInf, KeyInfos)
type SemInfAttach   = (ResAttrName, ATyVal)
-- type SemInfCast     = (ResAttrName, AttrName, ATy)
-- type SemInfosUnOp   = (ResAttrName, AttrName)   
-- type SemInfBinOp    = (ResAttrName, (LeftAttrName, RightAttrName))   
-- type SemInfFun1To1  = (FunTy1To1, ResAttrName, [AttrName])   
-- type SemInfFunAggr  = (FunTyAggr, SemInfosUnOp, Maybe PartAttrName)
-- type SemInfFunAggrCnt = (ResAttrName, Maybe PartAttrName)
-- type SemInfSerRel   = (AttrName, AttrName, [AttrName])

type AlgNode = (Algebra, [Int])

data Algebra where
--    RowNum     :: SemInfRowNum -> Algebra     -- Should have one child
--    RowId      :: SemInfRowId -> Algebra      -- should have one child
--    RowRank    :: SemInfRank -> Algebra       -- should have one child
--    Rank       :: SemInfRank -> Algebra       -- should have one child
    Proj       :: SemInfProj -> Algebra       -- should have one child   
--    Sel        :: SemInfSel  -> Algebra       -- should have one child  
--    PosSel     :: SemInfPosSel -> Algebra     -- should have one child
--    Cross      :: Algebra                     -- should have two children
    EqJoin     :: SemInfEqJoin -> Algebra     -- should have two children 
--    SemiJoin   :: SemInfEqJoin -> Algebra     -- should have two children 
--    ThetaJoin  :: SemInfThetaJoin -> Algebra  -- should have two children
--    DisjUnion  :: Algebra                     -- should have two children
--    Diff       :: Algebra                     -- should have two children
--    Distinct   :: Algebra                     -- should have one child
    LitTable   :: SemInfLitTable -> SchemaInfos -> Algebra
--    EmptyTable :: SchemaInf -> Algebra
--    TableRef   :: SemInfTableRef -> Algebra
    Attach     :: SemInfAttach -> Algebra     -- should have one child
--    Cast       :: SemInfCast -> Algebra       -- should have one child
--    FunNumEq   :: SemInfBinOp -> Algebra      -- should have one child
--    FunNumGt   :: SemInfBinOp -> Algebra      -- should have one child
--    Fun1To1    :: SemInfFun1To1 -> Algebra    -- should have one child
--    FunBoolAnd :: SemInfBinOp -> Algebra      -- should have one child      
--    FunBoolOr  :: SemInfBinOp -> Algebra      -- should have one child
--    FunBoolNot :: SemInfUnOp -> Algebra       -- should have one child
--    FunAggr    :: SemInfFunAggr -> Algebra    -- should have one child
--    FunAggrCnt :: SemInfFunAggrCnt -> Algebra -- should have one child
--    SerializeRel :: SemInfSerRel -> Algebra   -- should have two children
  deriving (Show, Eq, Ord)