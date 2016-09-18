module Database.Algebra.SQL.Query where

import           Data.Scientific
import qualified Data.Text          as T
import qualified Data.Time.Calendar as C

-- TODO Do we have to check for validity of types?
-- TODO is window clause standard?

-- | Mixed datatype for sequences of both types of queries.
data Query = QValueQuery
           { valueQuery :: ValueQuery
           }
           | QDefinitionQuery
           { definitionQuery :: DefinitionQuery
           } deriving Show

-- | A query which defines something (DDL).
data DefinitionQuery = -- CREATE MATERIALIZED VIEW foo AS ...
                       DQMatView
                     { sourceQuery :: ValueQuery
                     , viewName    :: String
                     }
                     -- A temporary table which is only existent in the current
                     -- session.
                     | DQTemporaryTable
                     { sourceQuery :: ValueQuery
                     , tTableName  :: String
                     } deriving Show

-- | A Query which has a table as a result.
data ValueQuery = VQSelect
                  -- The contained select statement.
                { selectStmt :: SelectStmt
                }
                  -- Literal tables (e.g. "VALUES (1, 2), (2, 4)").
                | VQLiteral
                { rows :: [[ColumnExpr]] -- ^ The values contained.
                }
                  -- The with query to bind value queries to names.
                | VQWith
                { -- | The bindings of the with query as a list of tuples, each
                  -- containing the table alias, the optional column names and
                  -- the used query.
                  cBindings :: [(String, Maybe [String], ValueQuery)]
                , cBody     :: ValueQuery
                }
                  -- A binary set operation
                  -- (e.g. "TABLE foo UNION ALL TABLE bar").
                | VQBinarySetOperation
                { leftQuery  :: ValueQuery -- ^ The left query.
                , rightQuery :: ValueQuery -- ^ The right query.
                  -- The used (multi-) set operation.
                , operation  :: SetOperation
                } deriving Show

-- | A (multi-) set operation for two sets.
data SetOperation = -- The union of two sets.
                    SOUnionAll
                    -- The difference of two sets.
                  | SOExceptAll
                  deriving Show

-- | Represents a SQL query using select and other optional clauses. The
-- reason this type is seperated (and not within VQSelect) is the use within
-- tile merging in earlier steps.
data SelectStmt = SelectStmt -- TODO do we need a window clause ?
                { -- | The constituents of the select clause.
                  selectClause  :: [SelectColumn] -- TODO should not be empty
                , -- | Indicates whether duplicates are removed.
                  distinct      :: Bool
                , -- | The constituents of the from clause.
                  fromClause    :: [FromPart]
                , -- | A list of conjunctive column expression.
                  whereClause   :: [ColumnExpr]
                , -- | The values to group by.
                  groupByClause :: [ColumnExpr]
                , -- | The values and direction to order the table after.
                  orderByClause :: [OrderExpr]
                } deriving Show

-- | Tells which column to sort by and in which direction.
data OrderExpr = OE
               { -- | The expression to order after.
                 oExpr         :: ExtendedExpr
               , sortDirection :: SortDirection
               } deriving Show

-- | The direction to sort in.
data SortDirection = Ascending
                   | Descending
                   deriving Show

-- | Represents a subset of possible statements which can occur in a from
-- clause.
data FromPart = -- Used as "... FROM foo AS bar ...", but also as
                -- "... FROM foo ...", where the table reference is the alias.
                FPAlias
              { fExpr      :: FromExpr        -- ^ The aliased expression.
              , fName      :: String          -- ^ The name of the alias.
              , optColumns :: Maybe [String]  -- ^ Optional column names.
              } deriving Show

-- A reference type used for placeholders.
type ReferenceType = Int

data FromExpr = -- Contains a subquery (e.g. "SELECT * FROM (TABLE foo) f;"),
                -- where "TABLE foo" is the sub query.
                FESubQuery
              { subQuery :: ValueQuery  -- ^ The sub query.
              }
              | -- A placeholder which is substituted later.
                FEVariable
              { vIdentifier :: ReferenceType
              }
                -- Reference to an existing table.
              | FETableReference
              { tableReferenceName :: String      -- ^ The name of the table.
              }
                -- Explicit join syntax
              | FEExplicitJoin
              { joinType :: JoinOperator
              , leftArg  :: FromPart
              , rightArg :: FromPart
              }
              deriving Show

data JoinOperator = LeftOuterJoin ColumnExpr
                  deriving Show



-- | Represents a subset of possible statements which can occur in a
-- select clause.
data SelectColumn = -- | @SELECT foo AS bar ...@
                    SCAlias
                  { sExpr :: ExtendedExpr -- ^ The value expression aliased.
                  , sName :: String       -- ^ The name of the alias.
                  }
                  | SCExpr ExtendedExpr
                  deriving Show

-- | Basic value expressions extended by aggregates and window functions.
data ExtendedExpr =
      -- | Encapsulates the base cases.
      EEBase
    { valueExpr :: ValueExprTemplate ExtendedExpr -- ^ The value expression.
    }
      -- | @f() OVER (PARTITION BY p ORDER BY s framespec)@
    | EEWinFun
    { -- | Function to be computed over the window
      winFun    :: WindowFunction
      -- | The expressions to partition by
    , partCols  :: [AggrExpr]
      -- | Optional partition ordering
    , orderBy   :: [WindowOrderExpr]
      -- | Optional frame specification
    , frameSpec :: Maybe FrameSpec
    }
      -- | Aggregate function expression.
    | EEAggrExpr
    { aggrExpr :: AggrExpr
    } deriving Show

-- | Shorthand for the value expression base part of 'ExtendedExpr'.
type ExtendedExprBase = ValueExprTemplate ExtendedExpr

-- | A special order expression, which is used in windows of window functions.
-- This is needed because we can use window functions in the ORDER BY clause,
-- but not in window functions.
data WindowOrderExpr = WOE
                       { woExpr         :: AggrExpr
                       , wSortDirection :: SortDirection
                       } deriving Show

data FrameSpec = FHalfOpen FrameStart
               | FClosed FrameStart FrameEnd
               deriving (Show)

-- | Window frame start specification
data FrameStart = FSUnboundPrec  -- ^ UNBOUNDED PRECEDING
                | FSValPrec Int  -- ^ <value> PRECEDING
                | FSCurrRow      -- ^ CURRENT ROW
                deriving (Show)

-- | Window frame end specification
data FrameEnd = FECurrRow    -- ^ CURRENT ROW
              | FEValFol Int -- ^ <value> FOLLOWING
              | FEUnboundFol -- ^ UNBOUNDED FOLLOWING
              deriving (Show)

-- | Window functions
data WindowFunction = WFMax ColumnExpr
                    | WFMin ColumnExpr
                    | WFSum ColumnExpr
                    | WFAvg ColumnExpr
                    | WFAll ColumnExpr
                    | WFAny ColumnExpr
                    | WFFirstValue ColumnExpr
                    | WFLastValue ColumnExpr
                    | WFCount
                    | WFRank
                    | WFDenseRank
                    | WFRowNumber
                    deriving (Show)

-- | Basic value expressions extended only by aggregates.
data AggrExpr = AEBase (ValueExprTemplate AggrExpr)
              | AEAggregate
              { aFunction :: AggregateFunction
              } deriving Show

-- | Shorthand for the value expression base part of 'AggrExpr'.
type AggrExprBase = ValueExprTemplate AggrExpr


-- | Aggregate functions.
data AggregateFunction = AFAvg ColumnExpr
                       | AFMax ColumnExpr
                       | AFMin ColumnExpr
                       | AFSum ColumnExpr
                       | AFCount ColumnExpr
                       | AFCountDistinct ColumnExpr
                       | AFCountStar
                       | AFAll ColumnExpr
                       | AFAny ColumnExpr
                       deriving Show

-- | A template which allows the definition of a mutual recursive type for value
-- expressions, such that it can be extended with further constructors by other
-- data definitions.
data ValueExprTemplate rec =
      -- | Encapsulates a representation of a SQL value.
      VEValue
    { value :: Value          -- ^ The value contained.
    }
      -- | A column.
    | VEColumn
    { cName   :: String         -- ^ The name of the column.
      -- | The optional prefix of the column.
    , cPrefix :: Maybe String
    }
     -- | Application of a binary function.
    | VEBinApp
    { binFun     :: BinaryFunction -- ^ The applied function.
    , firstExpr  :: rec            -- ^ The first operand.
    , secondExpr :: rec            -- ^ The second operand.
    }
    | VEUnApp
    { unFun :: UnaryFunction  -- ^ The applied function
    , arg   :: rec            -- ^ The operand
    }
      -- | e.g. @EXISTS (VALUES (1))@
    | VEExists
    { existsQuery :: ValueQuery     -- ^ The query to check on.
    }
      -- | e.g. @1 IN (VALUES (1))@
    | VEIn
    { inExpr  :: rec            -- ^ The value to check for.
    , inQuery :: ValueQuery     -- ^ The query to check in.
    }

      -- | @testExpr BETWEEN lowerExpr AND upperExpr@
    | VEBetween
    { testExpr :: rec
    , lowerExpr :: rec
    , upperExpr :: rec
    }
      -- | @CASE WHEN ELSE@ (restricted to one WHEN branch)
    | VECase
    { condExpr   :: rec
    , thenBranch :: rec
    , elseBranch :: rec
    } deriving Show
-- FIXME merge VECast and VENot into UnaryFunction (maybe not possible)

-- | A type which does not extend basic value expressions, and therefore can
-- appear in any SQL clause.
newtype ColumnExpr = CEBase (ValueExprTemplate ColumnExpr)
                     deriving Show

-- | Shorthand for the value expression base part of 'ColumnExpr'.
type ColumnExprBase = (ValueExprTemplate ColumnExpr)

-- | Types of binary functions.
data BinaryFunction = BFPlus
                    | BFMinus
                    | BFTimes
                    | BFDiv
                    | BFModulo
                    | BFContains
                    | BFSimilarTo
                    | BFLike
                    | BFConcat
                    | BFGreaterThan
                    | BFGreaterEqual
                    | BFLowerThan
                    | BFLowerEqual
                    | BFEqual
                    | BFNotEqual
                    | BFAnd
                    | BFOr
                    | BFCoalesce
                    deriving Show

-- | Types of unary functions
data UnaryFunction = UFSin
                   | UFCos
                   | UFTan
                   | UFASin
                   | UFACos
                   | UFATan
                   | UFSqrt
                   | UFExp
                   | UFLog
                   | UFLn
                   | UFSubString Integer Integer
                   | UFExtract ExtractField
                   | UFNot
                   -- A type cast (e.g. @CAST(1 AS DOUBLE PRECISION)@).
                   | UFCast DataType
                   | UFIsNull
                   deriving (Show)

-- | Fields that can be extracted from date/time types
data ExtractField = ExtractDay
                  | ExtractMonth
                  | ExtractYear
                  deriving (Show)

-- | Types of valid SQL 99 datatypes (most likely a small subset) as stated in
-- 'SQL 1999: Understanding Relational Language Components' (Melton, Simon)
data DataType = -- | @INTEGER@
                DTInteger
                -- | @DECIMAL@
              | DTDecimal
                -- | @DECIMAL(precision, scale)@
              | DTDecimalFixed Int Int
                -- | @DOUBLE PRECISION@
              | DTDoublePrecision
              | DTText
                -- | @BOOLEAN@
              | DTBoolean
                -- | @DATE@
              | DTDate
              deriving Show

data Value = -- | @42@
             VInteger Integer
             -- | Arbitrary precision numeric type
           | VDecimal Scientific
             -- | A double precision floating point number.
           | VDoublePrecision Double
             -- | e.g. @'foo'@
           | VText T.Text
             -- | e.g. @TRUE@, @FALSE@ (but not UNKOWN in this variant)
           | VBoolean Bool
             -- | Standard SQL dates (Gregorian calendar)
           | VDate C.Day
             -- | Representation of a null value. (While this can basically be
             -- part of any nullable type, it is added here for simplicity.
             -- Values aren't linked to types anyways.)
           | VNull
           deriving Show

--------------------------------------------------------------------------------

literalBaseExpr :: ValueExprTemplate r -> Bool
literalBaseExpr (VEValue _) = True
literalBaseExpr _           = False

literalAggrExpr :: AggrExpr -> Bool
literalAggrExpr (AEBase b) = literalBaseExpr b
literalAggrExpr _          = False
