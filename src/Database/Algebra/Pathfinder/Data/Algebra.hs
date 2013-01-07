{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
The Algebra module provides the internal datatypes used for
constructing algebaric plans. It is not recommended to use these
datatypes directly instead it is adviced the to use the functions
provided by the module Database.Algebra.Pathfinder.Algebra.Create
-}
module Database.Algebra.Pathfinder.Data.Algebra where

import           Numeric                     (showFFloat)

import           Database.Algebra.Aux
import           Database.Algebra.Dag        (Operator, opChildren, replaceOpChild)
import           Database.Algebra.Dag.Common

-- | The column data type is used to represent the table structure while
--  compiling ferry core into an PFAlgebraic plan
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

-- | PFAlgebraic types
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

-- | Show the PFAlgebraic types in a way that is compatible with
--  the xml plan.
instance Show ATy where
  show AInt     = "int"
  show AStr     = "str"
  show ABool    = "bool"
  show ADec     = "dec"
  show ADouble  = "dbl"
  show ANat     = "nat"
  show ASur     = "nat"

-- | Wrapper around values that can occur in an PFAlgebraic plan
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

-- | New and old attribute name
type ProjPair             = (NewAttrName, OldAttrName)

-- | Projection information, a list of new attribute names, and their old names.
type ProjInf              = [ProjPair]

-- | A tuple is a list of values
type Tuple = [AVal]

-- | Schema information, represents a table structure, the first element of the tuple is the column name the second its type.
type SchemaInfos = [(AttrName, ATy)]

type SemInfRowNum  = (ResAttrName, SortInf, Maybe PartAttrName)

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

-- | Information on how to perform a theta-join. The first element represents the column from the
-- first table that has to relate to the column in the second table represnted by the second
-- element in tuple. The third element represents the type of relation.
type SemInfThetaJoin = (LeftAttrName, RightAttrName, String)

-- | Information what to put in a literate table
type SemInfLitTable = [Tuple]

-- | Information for accessing a database table
type SemInfTableRef = (TableName, TableAttrInf, KeyInfos)

-- | Information what column, the first element, to attach to a table and what its content would be, the second element.
type SemInfAttach   = (ResAttrName, ATyVal)

type SemInfCast     = (ResAttrName, AttrName, ATy)

-- | Information on how to perform a binary operation
-- The first element is the function that is to be performed
-- The second element the column name for its result
-- The third element is the left argument for the operator
-- The fourth element is the right argument for the operator
type SemBinOp = (String, ResAttrName, LeftAttrName, RightAttrName)

type SemUnOp = (ResAttrName, AttrName)

type SemInfAggr  = ([(AggrType, ResAttrName, Maybe AttrName)], Maybe PartAttrName)

data NullOp = LitTable SemInfLitTable SchemaInfos
            | EmptyTable SchemaInfos
            | TableRef SemInfTableRef
            deriving (Ord, Eq, Show)

data UnOp = RowNum SemInfRowNum
          | RowRank SemInfRank
          | Rank SemInfRank
          | Proj SemInfProj
          | Sel SemInfSel
          | PosSel SemInfPosSel
          | Distinct ()
          | Attach SemInfAttach
          | FunBinOp SemBinOp
          | Cast SemInfCast
          | FunBoolNot SemUnOp
          | Aggr SemInfAggr
          | Dummy String
          deriving (Ord, Eq, Show)

data BinOp = Cross ()
           | EqJoin SemInfEqJoin
           | ThetaJoin SemInfThetaJoin
           | DisjUnion ()
           | Difference ()
           deriving (Ord, Eq, Show)

type PFAlgebra = Algebra () BinOp UnOp NullOp AlgNode

replaceChild :: forall t b u n c. Eq c => c -> c -> Algebra t b u n c -> Algebra t b u n c
replaceChild o n (TerOp op c1 c2 c3) = TerOp op (replace o n c1) (replace o n c2) (replace o n c3)
replaceChild o n (BinOp op c1 c2) = BinOp op (replace o n c1) (replace o n c2)
replaceChild o n (UnOp op c) = UnOp op (replace o n c)
replaceChild _ _ (NullaryOp op) = NullaryOp op

instance Operator PFAlgebra where
    opChildren (TerOp _ c1 c2 c3) = [c1, c2, c3]
    opChildren (BinOp _ c1 c2) = [c1, c2]
    opChildren (UnOp _ c) = [c]
    opChildren (NullaryOp _) = []

    replaceOpChild op old new = replaceChild old new op
