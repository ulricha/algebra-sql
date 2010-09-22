{-# LANGUAGE GADTs #-}
module Ferry.Algebra.Data.Algebra where

import Numeric (showFFloat)

-- | The column data type is used to represent the table structure while
--  compiling ferry core into an algebraic plan
--  The col column contains the column number and the type of its contents
--  The NCol column is used to group columns that together form an element of a record
-- , its string argument is used to represent the field name.
data Column where
    Col :: Int -> ATy -> Column
    NCol :: String -> Columns -> Column
  deriving (Show)

-- | One table can have multiple columns     
type Columns = [Column]

-- | Sorting rows in a direction
data SortDir = Asc
             | Desc
    deriving (Eq, Ord)
    
data AggrType = Avg 
              | Max 
              | Min 
              | Sum 
              | Count 
              | All 
              | Prod 
              | Dist
    deriving (Eq, Ord)

instance Show AggrType where
    show Avg      = "avg"
    show Max      = "max"
    show Min      = "min"
    show Sum      = "sum"
    show Count    = "count"
    show All      = "all"
    show Prod     = "prod"
    show Dist     = "distinct"

-- | The show instance results in values that are accepted in the xml plan.
instance Show SortDir where
    show Asc  = "ascending"
    show Desc = "descending"
    
--data JoinCompKind = TJ_EQ 
--                  | TJ_GT 
--                  | TJ_GE 
--                  | TJ_LT 
--                  | TJ_LE 
--                  | TJ_NE
--

-- | Algebraic types
--  At this level we do not have any structural types anymore
--  those are represented by columns. ASur is used for surrogate
--  values that occur for nested lists.
data ATy where
    AInt :: ATy             
    AStr :: ATy             
    ABool :: ATy
    ADec :: ATy             
    ADouble :: ATy          
    ANat :: ATy             
    ASur :: ATy
      deriving (Eq, Ord)
      
-- | Show the algebraic types in a way that is compatible with 
--  the xml plan.
instance Show ATy where
  show AInt     = "int"
  show AStr     = "str"
  show ABool    = "bool"
  show ADec     = "dec"
  show ADouble  = "dbl"
  show ANat     = "nat"
  show ASur     = "nat"

-- | Wrapper around values that can occur in an algebraic plan                  
data AVal where
  VInt :: Integer -> AVal
  VStr :: String -> AVal
  VBool :: Bool -> AVal
  VDouble :: Double -> AVal
  VDec :: Float -> AVal 
  VNat :: Integer -> AVal
    deriving (Eq, Ord)

-- | Show the values in the way compatible with the xml plan.
instance Show AVal where
  show (VInt x)     = show x
  show (VStr x)     = x 
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VDouble x)     =  show x
  show (VDec x)     = showFFloat (Just 2) x ""
  show (VNat x)     = show x

--
--data FunTyAggr = FTAggr_Avg
--               | FTAggr_Max
--               | FTAggr_Min
--               | FTAggr_Sum
--            deriving (Eq, Show)

-- | Pair of a type and a value
type ATyVal = (ATy, AVal)

-- | Attribute name or column name
type AttrName            = String

-- | Result attribute name, used as type synonym where the name for a result column of a computation is needed              
type ResAttrName         = AttrName

-- | Sort attribute name, used as type synonym where a column for sorting is needed
type SortAttrName        = AttrName

-- | Partition attribute name, used as a synonym where the name for the partitioning column is expected by the rownum operator
type PartAttrName        = AttrName

-- | New attribute name, used to represent the new column name when renaming columns
type NewAttrName         = AttrName

-- | Old attribute name, used to represent the old column name when renaming columns
type OldAttrName         = AttrName

-- | Selection attribute name, used to represent the column containing the selection boolean
type SelAttrName         = AttrName

-- | Left attribute name, used to represent the left argument when applying binary operators
type LeftAttrName        = AttrName

-- | Right attribute name, used to represent the right argument when applying binary operators
type RightAttrName       = AttrName
--
-- | Name of a database table
type TableName           = String  

-- | List of table attribute information consisting of (column name, new column name, type of column)
type TableAttrInf        = [(AttrName, AttrName, ATy)]

-- | Key of a database table, a key consists of multiple column names
type KeyInfo             = [AttrName]

-- | Multiple keys
type KeyInfos            = [KeyInfo]

-- | Sort information, a list (ordered in sorting priority), of pair of columns and their sort direction--
type SortInf              = [(SortAttrName, SortDir)]

-- | Projection information, a list of new attribute names, and their old names.
type ProjInf              = [(NewAttrName, OldAttrName)]  

--type JoinPred = (JoinCompKind, (LeftAttrName,RightAttrName))
--type JoinPreds  = [JoinPred]

-- | A tuple is a list of values
type Tuple = [AVal]

-- | Schema information, represents a table structure, the first element of the tuple is the column name the second its type.
type SchemaInfos = [(AttrName, ATy)]    

type SemInfRowNum  = (ResAttrName, SortInf, Maybe PartAttrName) 
-- type SemInfRowId   = ResAttrName

-- | Information that specifies how to perform the rank operation.
--  its first element is the column where the output of the operation is inserted
--  the second element represents the sorting criteria that determine the ranking.
type SemInfRank    = (ResAttrName,  SortInf)

-- | Information that specifies a projection
type SemInfProj    = ProjInf


-- | Information that specifies which column contains the conditional
type SemInfSel     = SelAttrName

-- | Information that specifies how to select element at a certain position
type SemInfPosSel  = (Int, SortInf, Maybe PartAttrName) 


-- | Information on how to perform an eq-join. The first element represents the column from the
-- first table that has to be equal to the column in the second table represented by the second
-- element in the pair.
type SemInfEqJoin  = (LeftAttrName,RightAttrName)


-- type SemInfThetaJoin = JoinPreds 

-- | Information what to put in a literate table
type SemInfLitTable = [Tuple]


-- | Information for accessing a database table
type SemInfTableRef = (TableName, TableAttrInf, KeyInfos)


-- | Information what column, the first element, to attach to a table and what its content would be, the second element.
type SemInfAttach   = (ResAttrName, ATyVal)


-- type SemInfCast     = (ResAttrName, AttrName, ATy)
-- type SemInfosUnOp   = (ResAttrName, AttrName)   
-- type SemInfBinOp    = (ResAttrName, (LeftAttrName, RightAttrName))   

-- | Information on how to perform a binary operation
-- The first element is the function that is to be performed
-- The second element the column name for its result
-- The third element is the left argument for the operator
-- The fourth element is the right argument for the operator
type SemBinOp = (String, ResAttrName, LeftAttrName, RightAttrName)

type SemUnOp = (ResAttrName, AttrName)

-- type SemInfFun1To1  = (FunTy1To1, ResAttrName, [AttrName])   

type SemInfAggr  = ([(AggrType, ResAttrName, Maybe AttrName)], Maybe PartAttrName)

-- type SemInfFunAggrCnt = (ResAttrName, Maybe PartAttrName)
-- type SemInfSerRel   = (AttrName, AttrName, [AttrName])


type AlgNode = Int

-- | An algebraic node is an algebraic element, and its children.
type AlgConstr = (Algebra, [AlgNode])


-- | Algebraic operations. These operation do not reference their own children directly
-- they only contain the information that is needed to perform the operation.
data Algebra where
    RowNum     :: SemInfRowNum -> Algebra     -- Should have one child
--    RowId      :: SemInfRowId -> Algebra      -- should have one child
    RowRank    :: SemInfRank -> Algebra       -- should have one child
    Rank       :: SemInfRank -> Algebra       -- should have one child
    Proj       :: SemInfProj -> Algebra       -- should have one child   
    Sel        :: SemInfSel  -> Algebra       -- should have one child  
    PosSel     :: SemInfPosSel -> Algebra     -- should have one child
    Cross      :: Algebra                     -- should have two children
    EqJoin     :: SemInfEqJoin -> Algebra     -- should have two children 
--    SemiJoin   :: SemInfEqJoin -> Algebra     -- should have two children 
--    ThetaJoin  :: SemInfThetaJoin -> Algebra  -- should have two children
    DisjUnion  :: Algebra                     -- should have two children
--    Diff       :: Algebra                     -- should have two children
    Distinct   :: Algebra                     -- should have one child
    LitTable   :: SemInfLitTable -> SchemaInfos -> Algebra
    EmptyTable :: SchemaInfos -> Algebra
    TableRef   :: SemInfTableRef -> Algebra
    Attach     :: SemInfAttach -> Algebra     -- should have one child
    FunBinOp   :: SemBinOp -> Algebra         -- should have one child
--    Cast       :: SemInfCast -> Algebra       -- should have one child
--    FunNumEq   :: SemInfBinOp -> Algebra      -- should have one child
--    FunNumGt   :: SemInfBinOp -> Algebra      -- should have one child
--    Fun1To1    :: SemInfFun1To1 -> Algebra    -- should have one child
--    FunBoolAnd :: SemInfBinOp -> Algebra      -- should have one child      
--    FunBoolOr  :: SemInfBinOp -> Algebra      -- should have one child
    FunBoolNot :: SemUnOp -> Algebra       -- should have one child
    Aggr       :: SemInfAggr -> Algebra    -- should have one child
--    FunAggrCnt :: SemInfFunAggrCnt -> Algebra -- should have one child
--    SerializeRel :: SemInfSerRel -> Algebra   -- should have two children
  deriving (Show, Eq, Ord)