{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A representation of table algebra operators over multiset
-- relations.
module Database.Algebra.Table.Lang where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Decimal
import           Data.Scientific
import           Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P
import           Text.Printf

import qualified Data.Text                    as T
import qualified Data.Time.Calendar           as C

import           Database.Algebra.Dag         (Operator, opChildren,
                                               replaceOpChild)
import           Database.Algebra.Dag.Common

-- | Sorting rows in a direction
data SortDir = Asc
             | Desc
    deriving (Eq, Ord, Read, Show)

data AggrType = Avg Expr
              | Max Expr
              | Min Expr
              | Sum Expr
              | CountStar
              | Count Expr
              | CountDistinct Expr
              | All Expr
              | Any Expr
    deriving (Eq, Ord, Show)

instance P.Pretty AggrType where
    pretty (Avg c)           = P.text "avg" <> P.parens (P.pretty c)
    pretty (Max c)           = P.text "max" <> P.parens (P.pretty c)
    pretty (Min c)           = P.text "min" <> P.parens (P.pretty c)
    pretty (Sum c)           = P.text "sum" <> P.parens (P.pretty c)
    pretty CountStar         = P.text "count(*)"
    pretty (Count c)         = P.text "count" <> P.parens (P.pretty c)
    pretty (CountDistinct c) = P.text "count_distinct" <> P.parens (P.pretty c)
    pretty (All c)           = P.text "all" <> P.parens (P.pretty c)
    pretty (Any c)           = P.text "any" <> P.parens (P.pretty c)

-- | The show instance results in values that are accepted in the xml plan.
instance P.Pretty SortDir where
    pretty Asc  = P.text "ascending"
    pretty Desc = P.text "descending"

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
    deriving (Eq, Ord, Show)

-- | Show the table algebra types in a way that is compatible with
--  the xml plan.
instance P.Pretty ATy where
    pretty ADate    = P.text "date"
    pretty AInt     = P.text "int"
    pretty AStr     = P.text "str"
    pretty ABool    = P.text "bool"
    pretty ADec     = P.text "dec"
    pretty ADouble  = P.text "dbl"

newtype Date = Date { unDate :: C.Day } deriving (Eq, Ord, Show)

-- | Wrapper around values that can occur in an table algebra plan
data AVal where
    VInt        :: Integer -> AVal
    VStr        :: T.Text -> AVal
    VBool       :: Bool -> AVal
    VDouble     :: Double -> AVal
    VDec        :: Scientific -> AVal
    VDate       :: Date -> AVal
    deriving (Eq, Ord, Show)

-- | Show the values in the way compatible with the xml plan.
instance P.Pretty AVal where
    pretty (VInt x)        = P.integer x
    pretty (VStr x)        = P.text $ T.unpack x
    pretty (VBool True)    = P.text "true"
    pretty (VBool False)   = P.text "false"
    pretty (VDouble x)     = P.double x
    pretty (VDec d)        = P.text $ show d
    pretty (VDate d)       = P.text $ C.showGregorian $ unDate d

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
newtype Key = Key [Attr] deriving (Eq, Ord, Show)

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
            deriving (Eq, Ord, Show)

instance P.Pretty BinFun where
    pretty Minus     = P.text $ "-"
    pretty Plus      = P.text $ "+"
    pretty Times     = P.text $ "*"
    pretty Div       = P.text $ "/"
    pretty Modulo    = P.text $ "%"
    pretty Contains  = P.text $ "fn:contains"
    pretty Concat    = P.text $ "fn:concat"
    pretty SimilarTo = P.text $ "fn:similar_to"
    pretty Like      = P.text $ "fn:like"
    pretty Gt        = P.text $ ">"
    pretty Lt        = P.text $ "<"
    pretty GtE       = P.text $ ">="
    pretty LtE       = P.text $ "<="
    pretty Eq        = P.text $ "=="
    pretty NEq       = P.text $ "<>"
    pretty And       = P.text $ "&&"
    pretty Or        = P.text $ "||"
    pretty Coalesce  = P.text $ "coalesce"

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
           | Ln
           | Exp
           | DateDay
           | DateYear
           | DateMonth
           | SubString Integer Integer
           | IsNull
           deriving (Eq, Ord, Show)

instance P.Pretty UnFun where
    pretty Not             = P.text $ "not"
    pretty (Cast ty)       = P.text "cast->" <> P.pretty ty
    pretty Sin             = P.text $ "sin"
    pretty Cos             = P.text $ "cos"
    pretty Tan             = P.text $ "tan"
    pretty Sqrt            = P.text $ "sqrt"
    pretty Exp             = P.text $ "exp"
    pretty Log             = P.text $ "log"
    pretty Ln              = P.text $ "ln"
    pretty ASin            = P.text $ "asin"
    pretty ACos            = P.text $ "acos"
    pretty ATan            = P.text $ "atan"
    pretty DateDay         = P.text $ "date_day"
    pretty DateYear        = P.text $ "date_year"
    pretty DateMonth       = P.text $ "date_month"
    pretty IsNull          = P.text $ "is_null"
    pretty (SubString f t) = P.text $ printf "subString_%d,%d" f t

data TernaryFun = If
                | Between
                deriving (Eq, Ord, Show)

instance P.Pretty TernaryFun where
    pretty If      = P.text "if"
    pretty Between = P.text "between"

-- | Projection expressions
data Expr = BinAppE BinFun Expr Expr
          | UnAppE UnFun Expr
          | ColE Attr
          | ConstE AVal
          | TernaryAppE TernaryFun Expr Expr Expr
          deriving (Eq, Ord, Show)

-- | Expressions which are used to specify partitioning in window
-- functions.
type PartExpr = Expr

parenthize :: Expr -> P.Doc
parenthize e =
    case e of
        ColE _   -> P.pretty e
        ConstE _ -> P.pretty e
        _        -> P.parens $ P.pretty e

instance P.Pretty Expr where
    pretty (BinAppE f e1 e2)        = parenthize e1 <+> P.pretty f <+> parenthize e2
    pretty (UnAppE f e)             = P.pretty f <+> (parenthize e)
    pretty (ColE c)                 = P.text c
    pretty (ConstE v)               = P.pretty v
    pretty (TernaryAppE f e1 e2 e3) = P.pretty f <+> parenthize e1
                                                 <+> parenthize e2
                                                 <+> parenthize e3

-- | New column name and the expression that generates the new column
type Proj                = (ResAttr, Expr)

-- | A tuple is a list of values
type Tuple = [AVal]

-- | Comparison operators which can be used for ThetaJoins.
data JoinRel = EqJ -- equal
             | GtJ -- greater than
             | GeJ -- greater equal
             | LtJ -- less than
             | LeJ -- less equal
             | NeJ -- not equal
             deriving (Eq, Ord, Show)

instance P.Pretty JoinRel where
    pretty EqJ = P.text "=="
    pretty GtJ = P.text ">"
    pretty GeJ = P.text ">="
    pretty LtJ = P.text "<"
    pretty LeJ = P.text "<="
    pretty NeJ = P.text "/="

-- | Window frame start specification
data FrameStart = FSUnboundPrec  -- ^ UNBOUNDED PRECEDING
                | FSValPrec Int  -- ^ <value> PRECEDING
                | FSCurrRow      -- ^ CURRENT ROW
                deriving (Eq, Ord, Show)

-- | Window frame end specification
data FrameEnd = FECurrRow    -- ^ CURRENT ROW
              | FEValFol Int -- ^ <value> FOLLOWING
              | FEUnboundFol -- ^ UNBOUNDED FOLLOWING
              deriving (Eq, Ord, Show)

data FrameBounds = HalfOpenFrame FrameStart
                 | ClosedFrame FrameStart FrameEnd
                 deriving (Eq, Ord, Show)

data WinFun = WinMax Expr
            | WinMin Expr
            | WinSum Expr
            | WinAvg Expr
            | WinAll Expr
            | WinAny Expr
            | WinFirstValue Expr
            | WinLastValue Expr
            | WinCount
            deriving (Eq, Ord, Show)


data NullOp = LitTable ([Tuple], [TypedAttr])
            | TableRef (TableName, [TypedAttr], [Key])
            deriving (Ord, Eq, Show)

data PayloadCol = PayloadCol Attr Expr deriving (Ord, Eq, Show)
data OrdCol     = OrdCol (Attr, SortDir) Expr deriving (Ord, Eq, Show)
data KeyCol     = KeyCol Attr Expr deriving (Ord, Eq, Show)
data RefCol     = RefCol Attr Expr deriving (Ord, Eq, Show)

instance P.Pretty PayloadCol where
    pretty (PayloadCol c e) = P.text c <> P.colon <> P.pretty e

instance P.Pretty OrdCol where
    pretty (OrdCol (c, Asc) e)  = P.text c <> P.text ".asc:" <> P.pretty e
    pretty (OrdCol (c, Desc) e) = P.text c <> P.text ".desc:" <> P.pretty e

instance P.Pretty KeyCol where
    pretty (KeyCol c e) = P.text c <> P.text ":" <> P.pretty e

instance P.Pretty RefCol where
    pretty (RefCol c e) = P.text c <> P.text ":" <> P.pretty e

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
          deriving (Ord, Eq, Show)

data BinOp = Cross ()
           | EqJoin (LeftAttr,RightAttr)
           | ThetaJoin [(Expr, Expr, JoinRel)]
           | LeftOuterJoin [(Expr, Expr, JoinRel)]
           | SemiJoin [(Expr, Expr, JoinRel)]
           | AntiJoin [(Expr, Expr, JoinRel)]
           | DisjUnion ()
           | Difference ()
           deriving (Ord, Eq, Show)

type TableAlgebra = Algebra () BinOp UnOp NullOp AlgNode

instance FromJSON Date where
    parseJSON o = Date <$> (\(y, m, d) -> C.fromGregorian y m d) <$> parseJSON o

instance ToJSON Date where
    toJSON = toJSON . C.toGregorian . unDate

instance ToJSON Decimal where
    toJSON = toJSON . show

instance FromJSON Decimal where
    parseJSON s = read <$> parseJSON s

--------------------------------------------------------------------------------
-- Aeson instances for JSON serialization

deriveJSON defaultOptions ''AggrType
deriveJSON defaultOptions ''ATy
deriveJSON defaultOptions ''AVal
deriveJSON defaultOptions ''SortDir
deriveJSON defaultOptions ''JoinRel
deriveJSON defaultOptions ''NullOp
deriveJSON defaultOptions ''WinFun
deriveJSON defaultOptions ''UnOp
deriveJSON defaultOptions ''BinOp
deriveJSON defaultOptions ''Expr
deriveJSON defaultOptions ''UnFun
deriveJSON defaultOptions ''BinFun
deriveJSON defaultOptions ''TernaryFun
deriveJSON defaultOptions ''Key
deriveJSON defaultOptions ''RefCol
deriveJSON defaultOptions ''KeyCol
deriveJSON defaultOptions ''OrdCol
deriveJSON defaultOptions ''PayloadCol
deriveJSON defaultOptions ''FrameBounds
deriveJSON defaultOptions ''FrameEnd
deriveJSON defaultOptions ''FrameStart
