{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
--new, required for JSON
{-# LANGUAGE DeriveGeneric #-}

-- | The Algebra module provides the internal datatypes used for
-- constructing algebaric plans. It is not recommended to use these
-- datatypes directly instead it is adviced to use the functions
-- provided by the module Database.Algebra.Pathfinder.Algebra.Create
module Database.Algebra.Pathfinder.Data.Algebra where

import Numeric                     (showFFloat)
import Text.Printf

import Database.Algebra.Aux
import Database.Algebra.Dag        (Operator, opChildren, replaceOpChild)
import Database.Algebra.Dag.Common

-- required for JSON
import GHC.Generics (Generic)

-- | Sorting rows in a direction
data SortDir = Asc
             | Desc
    deriving (Eq, Ord, Generic, Read)

data AggrType = Avg AttrName
              | Max AttrName
              | Min AttrName
              | Sum AttrName
              | Count
              | All AttrName
              | Prod AttrName
              | Dist AttrName
    deriving (Eq, Ord, Generic)

instance Show AggrType where
    show (Avg c)  = printf "avg(%s)" c
    show (Max c)  = printf "max(%s)" c
    show (Min c)  = printf "min(%s)" c
    show (Sum c)  = printf "sum(%s)" c
    show Count    = "count"
    show (All c)  = printf "all(%s)" c
    show (Prod c) = printf "prod(%s)" c
    show (Dist c) = printf "dist(%s)" c

-- | The show instance results in values that are accepted in the xml plan.
instance Show SortDir where
    show Asc  = "ascending"
    show Desc = "descending"

-- | PFAlgebraic types
--  At this level we do not have any structural types anymore
--  those are represented by columns. 
data ATy where
    AInt :: ATy
    AStr :: ATy
    ABool :: ATy
    ADec :: ATy
    ADouble :: ATy
    ANat :: ATy
    deriving (Eq, Ord, Generic)

-- | Show the PFAlgebraic types in a way that is compatible with
--  the xml plan.
instance Show ATy where
  show AInt     = "int"
  show AStr     = "str"
  show ABool    = "bool"
  show ADec     = "dec"
  show ADouble  = "dbl"
  show ANat     = "nat"

-- | Wrapper around values that can occur in an PFAlgebraic plan
data AVal where
  VInt    :: Integer -> AVal
  VStr    :: String -> AVal
  VBool   :: Bool -> AVal
  VDouble :: Double -> AVal
  VDec    :: Float -> AVal
  VNat    :: Integer -> AVal
    deriving (Eq, Ord, Generic)

-- | Show the values in the way compatible with the xml plan.
instance Show AVal where
  show (VInt x)     = show x
  show (VStr x)     = x
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VDouble x)     =  show x
  show (VDec x)     = showFFloat (Just 2) x ""
  show (VNat x)     = show x

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

-- | Left attribute name, used to represent the left argument when applying binary operators
type LeftAttrName        = AttrName

-- | Right attribute name, used to represent the right argument when applying binary operators
type RightAttrName       = AttrName
--
-- | Name of a database table
type TableName           = String

-- | Typed columns
type TypedAttr = (AttrName, ATy)

-- | Key of a database table, a key consists of multiple column names
newtype Key = Key [AttrName] deriving (Eq, Ord, Show, Generic)

-- | Sort information, a list (ordered in sorting priority), of pair of columns and their sort direction--
type SortInf              = [(SortAttrName, SortDir)]

-- | Binary functions and operators in expressions
data BinFun = Gt
            | Lt
            | GtE
            | LtE
            | Eq
            | And
            | Or
            | Plus
            | Minus
            | Times
            | Div
            | Modulo
            | Contains
            | SimilarTo
            | Like
            | Concat
            deriving (Eq, Ord, Generic)

instance Show BinFun where
  show Minus     = "-"
  show Plus      = "+"
  show Times     = "*"
  show Div       = "/"
  show Modulo    = "%"
  show Contains  = "fn:contains"
  show Concat    = "fn:concat"
  show SimilarTo = "fn:similar_to"
  show Like      = "fn:like"
  show Gt        = ">"
  show Lt        = "<"
  show GtE       = ">="
  show LtE       = "<="
  show Eq        = "=="
  show And       = "&&"
  show Or        = "||"
  
-- | Unary functions/operators in expressions
data UnFun = Not
           | Cast ATy
           deriving (Eq, Ord, Generic)

instance Show UnFun where
  show Not       = "not"
  show (Cast ty) = "cast->" ++ show ty

-- | Projection expressions
data Expr = BinAppE BinFun Expr Expr
          | UnAppE UnFun Expr
          | ColE AttrName
          | ConstE AVal
          deriving (Eq, Ord, Generic)
              
instance Show Expr where
  show (BinAppE f e1 e2) = "(" ++ show e1 ++ ") " ++ show f ++ " (" ++ show e2 ++ ")"
  show (UnAppE f e)      = show f ++ "(" ++ show e ++ ")"
  show (ColE c)          = c
  show (ConstE v)        = show v

-- | New column name and the expression that generates the new column
type Proj                = (NewAttrName, Expr)

-- | A tuple is a list of values
type Tuple = [AVal]

-- | Schema information, represents a table structure, the first element of the
-- tuple is the column name the second its type.
type SchemaInfos = [(AttrName, ATy)]

type SemInfRowNum  = (ResAttrName, SortInf, Maybe PartAttrName)

-- | Information that specifies how to perform the rank operation.  its first
--  element is the column where the output of the operation is inserted the
--  second element represents the sorting criteria that determine the ranking.
type SemInfRank    = (ResAttrName,  SortInf)

-- | Information that specifies how to select element at a certain position
type SemInfPosSel  = (Int, SortInf, Maybe PartAttrName)

-- | Information on how to perform an eq-join. The first element represents the
-- column from the first table that has to be equal to the column in the second
-- table represented by the second element in the pair.
type SemInfEqJoin  = (LeftAttrName,RightAttrName)

-- | Information on how to perform a theta-join. The first element represents
-- the column from the first table that has to relate to the column in the
-- second table represnted by the second element in tuple. The third element
-- represents the type of relation.
type SemInfJoin = [(LeftAttrName, RightAttrName, JoinRel)]

-- | Comparison operators which can be used for ThetaJoins.
data JoinRel = EqJ -- equal
             | GtJ -- greater than
             | GeJ -- greater equal
             | LtJ -- less than
             | LeJ -- less equal
             | NeJ -- not equal
             deriving (Eq, Ord, Generic)
             
instance Show JoinRel where
  show EqJ = "eq"
  show GtJ = "gt"
  show GeJ = "ge"
  show LtJ = "lt"
  show LeJ = "le"
  show NeJ = "ne"

-- | Information what to put in a literate table
type SemInfLitTable = [Tuple]

-- | Information for accessing a database table
type SemInfTableRef = (TableName, [TypedAttr], [Key])

type SemInfCast     = (ResAttrName, AttrName, ATy)

type SemUnOp = (ResAttrName, AttrName)

type SemInfAggr  = ([(AggrType, ResAttrName)], [PartAttrName])

data NullOp = LitTable SemInfLitTable SchemaInfos
            -- FIXME Separate EmptyTables are not necessary -> eliminate
            | EmptyTable SchemaInfos
            | TableRef SemInfTableRef
            deriving (Ord, Eq, Show, Generic)

data UnOp = RowNum SemInfRowNum
          | RowRank SemInfRank
          | Rank SemInfRank
          | Project [(AttrName, Expr)]
          | Select Expr
          -- What exactly is the semantics here?
          -- FIXME eliminate
          | PosSel SemInfPosSel
          | Distinct ()
          | Aggr SemInfAggr
          deriving (Ord, Eq, Show, Generic)

data BinOp = Cross ()
           | EqJoin SemInfEqJoin
           | ThetaJoin SemInfJoin
           | SemiJoin SemInfJoin
           | AntiJoin SemInfJoin
           | DisjUnion ()
           | Difference ()
           deriving (Ord, Eq, Show, Generic)

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
