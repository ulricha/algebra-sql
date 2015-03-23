{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A representation of table algebra operators over multiset
-- relations.
module Database.Algebra.Table.Lang where

import           Control.Applicative
import           Data.Aeson
import           Data.Decimal
import           Data.List
import qualified Data.Time.Calendar          as C
import           Text.Printf

import           Database.Algebra.Dag        (Operator, opChildren,
                                              replaceOpChild)
import           Database.Algebra.Dag.Common

-- required for JSON
import           GHC.Generics                (Generic)

-- | Sorting rows in a direction
data SortDir = Asc | Desc
    deriving (Eq, Ord, Generic, Read)

data AggrType = Avg Expr
              | Max Expr
              | Min Expr
              | Sum Expr
              | Count
              | All Expr
              | Any Expr
    deriving (Eq, Ord, Generic)

instance Show AggrType where
    show (Avg c)  = printf "avg(%s)" (show c)
    show (Max c)  = printf "max(%s)" (show c)
    show (Min c)  = printf "min(%s)" (show c)
    show (Sum c)  = printf "sum(%s)" (show c)
    show Count    = "count"
    show (All c)  = printf "all(%s)" (show c)
    show (Any c)  = printf "any(%s)" (show c)

-- | The show instance results in values that are accepted in the xml plan.
instance Show SortDir where
    show Asc  = "ascending"
    show Desc = "descending"

-- | table algebra types
--  At this level we do not have any structural types anymore
--  those are represented by columns.
data ATy where
    AInt :: ATy
    AStr :: ATy
    ABool :: ATy
    ADec :: ATy
    ADouble :: ATy
    ADate :: ATy
    deriving (Eq, Ord, Generic)

-- | Show the table algebra types in a way that is compatible with
--  the xml plan.
instance Show ATy where
  show ADate    = "date"
  show AInt     = "int"
  show AStr     = "str"
  show ABool    = "bool"
  show ADec     = "dec"
  show ADouble  = "dbl"

-- | Wrapper around values that can occur in an table algebra plan
data AVal where
  VInt    :: Integer -> AVal
  VStr    :: String -> AVal
  VBool   :: Bool -> AVal
  VDouble :: Double -> AVal
  VDec    :: Decimal -> AVal
  VDate   :: C.Day -> AVal
  deriving (Eq, Ord, Generic)

-- | Show the values in the way compatible with the xml plan.
instance Show AVal where
  show (VInt x)      = show x
  show (VStr x)      = x
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VDouble x)   = show x
  show (VDec d)      = show d
  show (VDate d)     = C.showGregorian d

-- | Attribute name or column name
type Attr            = String

-- | Name of an attribute in which the result of an expression,
-- aggregate or window function is stored.
type ResAttr         = Attr

-- | Names of partition attributes used in window specifications
type PartAttr        = Attr

-- | Left attribute name, used to represent the left argument when
-- applying binary operators
type LeftAttr        = Attr

-- | Right attribute name, used to represent the right argument when
-- applying binary operators
type RightAttr       = Attr
--
-- | Name of a database table
type TableName           = String

-- | Typed columns
type TypedAttr = (Attr, ATy)

-- | Key of a database table, a key consists of multiple column names
newtype Key = Key [Attr] deriving (Eq, Ord, Show, Generic)

-- | Sorting information
type SortSpec              = (Expr, SortDir)

-- | Binary functions and operators in expressions
data BinFun = Gt
            | Lt
            | GtE
            | LtE
            | Eq
            | NEq
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
            | Coalesce
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
  show NEq       = "<>"
  show And       = "&&"
  show Or        = "||"
  show Coalesce  = "coalesce"

-- | Unary functions/operators in expressions
data UnFun = Not
           | Cast ATy
           | Sin
           | Cos
           | Tan
           | ASin
           | ACos
           | ATan
           | Sqrt
           | Log
           | Exp
           | DateDay
           | DateYear
           | DateMonth
           | SubString Integer Integer
           | IsNull
           deriving (Eq, Ord, Generic)

instance Show UnFun where
  show Not             = "not"
  show (Cast ty)       = "cast->" ++ show ty
  show Sin             = "sin"
  show Cos             = "cos"
  show Tan             = "tan"
  show Sqrt            = "sqrt"
  show Exp             = "exp"
  show Log             = "log"
  show ASin            = "asin"
  show ACos            = "acos"
  show ATan            = "atan"
  show DateDay         = "date_day"
  show DateYear        = "date_year"
  show DateMonth       = "date_month"
  show IsNull          = "is_null"
  show (SubString f t) = printf "subString_%d,%d" f t

-- | Projection expressions
data Expr = BinAppE BinFun Expr Expr
          | UnAppE UnFun Expr
          | ColE Attr
          | ConstE AVal
          | IfE Expr Expr Expr
          deriving (Eq, Ord, Generic)

-- | Expressions which are used to specify partitioning in window
-- functions.
type PartExpr = Expr

instance Show Expr where
  show (BinAppE f e1 e2) = "(" ++ show e1 ++ ") " ++ show f ++ " (" ++ show e2 ++ ")"
  show (UnAppE f e)      = show f ++ "(" ++ show e ++ ")"
  show (ColE c)          = c
  show (ConstE v)        = show v
  show (IfE c t e)       = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e

-- | New column name and the expression that generates the new column
type Proj                = (ResAttr, Expr)

-- | A tuple is a list of values
type Tuple = [AVal]

-- | Schema information, represents a table structure, the first element of the
-- tuple is the column name the second its type.
type SchemaInfos = [(Attr, ATy)]

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

-- | Window frame start specification
data FrameStart = FSUnboundPrec  -- ^ UNBOUNDED PRECEDING
                | FSValPrec Int  -- ^ <value> PRECEDING
                | FSCurrRow      -- ^ CURRENT ROW
                deriving (Eq, Ord, Show, Generic)

-- | Window frame end specification
data FrameEnd = FECurrRow    -- ^ CURRENT ROW
              | FEValFol Int -- ^ <value> FOLLOWING
              | FEUnboundFol -- ^ UNBOUNDED FOLLOWING
              deriving (Eq, Ord, Show, Generic)

data FrameBounds = HalfOpenFrame FrameStart
                 | ClosedFrame FrameStart FrameEnd
                 deriving (Eq, Ord, Show, Generic)

data WinFun = WinMax Expr
            | WinMin Expr
            | WinSum Expr
            | WinAvg Expr
            | WinAll Expr
            | WinAny Expr
            | WinFirstValue Expr
            | WinLastValue Expr
            | WinCount
            deriving (Eq, Ord, Show, Generic)


data NullOp = LitTable ([Tuple], SchemaInfos)
            | TableRef (TableName, [TypedAttr], [Key])
            deriving (Ord, Eq, Show, Generic)

newtype DescrCol   = DescrCol Attr deriving (Ord, Eq, Generic)

instance Show DescrCol where
    show (DescrCol c) = "Descr " ++ c

-- | Declare need for position columns in the query result. The
-- distinction between AbsPos and RelPos is only relevant for the
-- optimizer: AbsPos signals that the actual pos values are
-- required. RelPos signals that only the order induced by the pos
-- column is relevant.
data SerializeOrder = AbsPos Attr
                    | RelPos [Attr]
                    | NoPos
                    deriving (Ord, Eq, Generic)

instance Show SerializeOrder where
    show (AbsPos c)  = "AbsPos " ++ c
    show (RelPos cs) = "RelPos " ++ (intercalate ", " cs)
    show NoPos       = "NoPos"

newtype PayloadCol = PayloadCol Attr deriving (Ord, Eq, Generic)
newtype OrdCol     = OrdCol Attr deriving (Ord, Eq, Generic)
newtype KeyCol     = KeyCol Attr deriving (Ord, Eq, Generic)
newtype RefCol     = RefCol Attr deriving (Ord, Eq, Generic)

instance Show PayloadCol where
    show (PayloadCol c) = c

instance Show OrdCol where
    show (OrdCol c) = c

instance Show KeyCol where
    show (KeyCol c) = c

instance Show RefCol where
    show (RefCol c) = c

data UnOp = RowNum (Attr, [SortSpec], [PartExpr])
          | RowRank (ResAttr, [SortSpec])
          | WinFun ((ResAttr, WinFun), [PartExpr], [SortSpec], Maybe FrameBounds)
          | Rank (ResAttr, [SortSpec])
          | Project [(Attr, Expr)]
          | Select Expr
          | Distinct ()
          | Aggr ([(AggrType, ResAttr)], [(PartAttr, Expr)])

          -- Serialize must only occur as the root node of a query. It
          -- defines physical order, natural key and reference columns
          -- of the query result.
          | Serialize ([RefCol], [KeyCol], [OrdCol], [PayloadCol])
          deriving (Ord, Eq, Show, Generic)

data BinOp = Cross ()
           | EqJoin (LeftAttr,RightAttr)
           | ThetaJoin [(Expr, Expr, JoinRel)]
           | LeftOuterJoin [(Expr, Expr, JoinRel)]
           | SemiJoin [(Expr, Expr, JoinRel)]
           | AntiJoin [(Expr, Expr, JoinRel)]
           | DisjUnion ()
           | Difference ()
           deriving (Ord, Eq, Show, Generic)

type TableAlgebra = Algebra () BinOp UnOp NullOp AlgNode

replace :: Eq a => a -> a -> a -> a
replace orig new x = if x == orig then new else x

replaceChild :: forall t b u n c. Eq c => c -> c -> Algebra t b u n c -> Algebra t b u n c
replaceChild o n (TerOp op c1 c2 c3) = TerOp op (replace o n c1) (replace o n c2) (replace o n c3)
replaceChild o n (BinOp op c1 c2) = BinOp op (replace o n c1) (replace o n c2)
replaceChild o n (UnOp op c) = UnOp op (replace o n c)
replaceChild _ _ (NullaryOp op) = NullaryOp op

instance Operator TableAlgebra where
    opChildren (TerOp _ c1 c2 c3) = [c1, c2, c3]
    opChildren (BinOp _ c1 c2) = [c1, c2]
    opChildren (UnOp _ c) = [c]
    opChildren (NullaryOp _) = []

    replaceOpChild op old new = replaceChild old new op

instance FromJSON C.Day where
    parseJSON o = (\(y, m, d) -> C.fromGregorian y m d) <$> parseJSON o

instance ToJSON C.Day where
    toJSON = toJSON . C.toGregorian

instance ToJSON Decimal where
    toJSON = toJSON . show

instance FromJSON Decimal where
    parseJSON s = read <$> parseJSON s

-- FIXME use TH derivation to improve compilation time.
instance ToJSON ATy where
instance ToJSON AVal where
instance ToJSON SortDir where
instance ToJSON JoinRel where
instance ToJSON SortSpec where
instance ToJSON AggrType where
instance ToJSON NullOp where
instance ToJSON WinFun where
instance ToJSON UnOp where
instance ToJSON BinOp where
instance ToJSON Expr where
instance ToJSON UnFun where
instance ToJSON BinFun where
instance ToJSON Key where
instance ToJSON DescrCol where
instance ToJSON SerializeOrder where
instance ToJSON PayloadCol where
instance ToJSON OrdCol where
instance ToJSON KeyCol where
instance ToJSON RefCol where
instance ToJSON FrameBounds where
instance ToJSON FrameEnd where
instance ToJSON FrameStart where

instance FromJSON ATy where
instance FromJSON AVal where
instance FromJSON SortDir where
instance FromJSON JoinRel where
instance FromJSON SortSpec where
instance FromJSON AggrType where
instance FromJSON NullOp where
instance FromJSON WinFun where
instance FromJSON UnOp where
instance FromJSON BinOp where
instance FromJSON Expr where
instance FromJSON UnFun where
instance FromJSON BinFun where
instance FromJSON Key where
instance FromJSON DescrCol where
instance FromJSON SerializeOrder where
instance FromJSON PayloadCol where
instance FromJSON OrdCol where
instance FromJSON KeyCol where
instance FromJSON RefCol where
instance FromJSON FrameBounds where
instance FromJSON FrameEnd where
instance FromJSON FrameStart where
