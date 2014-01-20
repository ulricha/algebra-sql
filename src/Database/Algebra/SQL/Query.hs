module Database.Algebra.SQL.Query where

-- TODO Do we have to check for validity of types?

-- TODO is window clause standard?
-- TODO what is VNat/ANat/ASur needed for
-- TODO add syntax for IN with lists
-- TODO maybe change whereClause to a conjuction list? would increase
-- readability

-- | Mixed datatype for sequences of both types of queries.
data Query = QValueQuery
           { valueQuery      :: ValueQuery
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
                { rows       :: [[ValueExpr]]  -- ^ The values contained.
                }
                  -- The with clause to bind value queries to names.
                | VQCommonTableExpression
                { cBody      :: ValueQuery

                  -- | The bindings of the CTE as a list of tuples, each
                  -- containing the table alias, the optional column names and
                  -- the used query.
                , cBindings  :: [(String, Maybe [String], ValueQuery)]
                }
                  -- A binary set operation
                  -- (e.g. "TABLE foo UNION ALL TABLE bar").
                | VQBinarySetOperation
                { leftQuery  :: ValueQuery     -- ^ The left query.
                , rightQuery :: ValueQuery     -- ^ The right query.
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

                  -- | Indicates whether duplicates are removed.
                , distinct      :: Bool

                  -- | The constituents of the from clause.
                , fromClause    :: [FromPart]   

                  -- | Conditional expression in the where clause.
                , whereClause   :: Maybe ValueExpr
                
                  -- | The values to group by.
                , groupByClause :: [ValueExpr]
                
                  -- | The values and direction to order the table after.
                , orderByClause :: [OrderExpr]
                } deriving Show

-- | Tells which column to sort by and in which direction.
data OrderExpr = OE
               { -- | The expression to order after.
                 oExpr         :: ValueExpr
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
              { fExpr          :: FromExpr        -- ^ The aliased expression.
              , fName          :: String          -- ^ The name of the alias.
              , optColumns     :: Maybe [String]  -- ^ Optional column names.
              }
                -- Inner join of two from parts. TODO always aliased? maybe remove
              | FPInnerJoin
              { leftPart       :: FromPart        -- ^ The left part of the join.
              , rightPart      :: FromPart        -- ^ The right part of the join.
              , joinCondition  :: ValueExpr       -- ^ The join condition.
              } deriving Show

-- A reference type used for placeholders.
type ReferenceType = Int

data FromExpr = -- Contains a subquery (e.g. "SELECT * FROM (TABLE foo) f;"),
                -- where "TABLE foo" is the sub query.
                FESubQuery
              { subQuery           :: ValueQuery  -- ^ The sub query.
              }
              | -- A placeholder which is substituted later.
                FEVariable
              { vIdentifier        :: ReferenceType
              }
                -- Reference to an existing table.
              | FETableReference
              { tableReferenceName :: String      -- ^ The name of the table.
              } deriving Show

              

-- | Represents a subset of possible statements which can occur in a
-- select clause.
data SelectColumn = -- | @SELECT foo AS bar ...@
                    SCAlias
                  { sExpr    :: SelectExpr  -- ^ The value expression aliased.
                  , sName    :: String      -- ^ The name of the alias.
                  } deriving Show

-- | An expression which can only occur within a select clause.
data SelectExpr = -- | Encapsulates a value expression.
                  SEValueExpr
                { valueExpr   :: ValueExpr    -- ^ The value expression.
                }
                  -- | @ROW_NUMBER() OVER (PARTITION BY p ORDER BY ...)@
                | SERowNum
                {  -- | The expression to partition by.
                  optPartCol  :: Maybe ValueExpr
                , orderBy     :: [OrderExpr]  -- ^ Order information.
                }
                  -- | @DENSE_RANK() OVER (ORDER BY ...)@
                | SEDenseRank
                { orderBy     :: [OrderExpr]
                }
                | SERank
                { orderBy     :: [OrderExpr]
                }
                  -- | Aggregate function expression. 
                | SEAggregate
                { -- | The optional value expression used (e.g. @COUNT@ does
                  -- not need one).
                  optValueExpr :: Maybe ValueExpr
                , -- | The function used to form the aggregate.
                  aFunction    :: AggregateFunction
                } deriving Show

-- | Aggregate functions.
data AggregateFunction = AFAvg
                       | AFMax
                       | AFMin
                       | AFSum
                       | AFCount
                       | AFAll
                       | AFProd
                       | AFDist
                       deriving Show

-- | Represents a value expression which can occur within several SQL parts.
data ValueExpr = -- | Encapsulates a representation of a SQL value.
                 VEValue
               { value        :: Value          -- ^ The value contained.
               }
                 -- | A column.
               | VEColumn
               { cName        :: String         -- ^ The name of the column.
                 -- | The optional prefix of the column.
               , cPrefix      :: Maybe String
               }
                 -- | A type cast (e.g. @CAST(1 AS DOUBLE PRECISION)@).
                 --     
                 -- 
               | VECast
               { target       :: ValueExpr      -- ^ The target of the cast.
               , type_        :: DataType       -- ^ The type to cast into.
               }
                 -- | Application of a binary function.
               | VEBinApp
               { function     :: BinaryFunction -- ^ The applied function.
               , firstExpr    :: ValueExpr      -- ^ The first operand.
               , secondExpr   :: ValueExpr      -- ^ The second operand.
               }
                 -- | Application of the not function.
               | VENot
               { nTarget      :: ValueExpr      -- ^ The expression to negate.
               }
                 -- | e.g. @EXISTS (VALUES (1))@
               | VEExists
               { existsQuery  :: ValueQuery     -- ^ The query to check on.
               }
                 -- | e.g. @1 IN (VALUES (1))@
               | VEIn
               { inExpr       :: ValueExpr      -- ^ The value to check for.
               , inQuery      :: ValueQuery     -- ^ The query to check in.
               } deriving Show

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
                    deriving Show

-- | Types of valid SQL 99 datatypes (most likely a small subset) as stated in
-- 'SQL 1999: Understanding Relational Language Components' (Melton, Simon)
data DataType = -- | @INTEGER@
                DTInteger
                -- | @DECIMAL@
              | DTDecimal
                -- | @DOUBLE PRECISION@
              | DTDoublePrecision
              | DTText
                -- | @BOOLEAN@
              | DTBoolean
              deriving Show

data Value = -- | @42@
             VInteger Integer
             -- | Numeric data type with fixed precision and scale (e.g. @1.4@) 
           | VDecimal Float
             -- | A double precision floating point number. 
           | VDoublePrecision Double
             -- | e.g. @'foo'@
           | VText String
             -- | e.g. @TRUE@, @FALSE@ (but not UNKOWN in this variant)
           | VBoolean Bool
             -- | Representation of a null value. (While this can basically be
             -- part of any nullable type, it is added here for simplicity.
             -- Values aren't linked to types anyways.)
           | VNull
           deriving Show

