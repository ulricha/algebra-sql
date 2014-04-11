-- | Datatypes and functions which determine mergability of SQL fragements.
module Data.Algebra.SQL.Mergability
    ( Clause
    ) where

-- | Specifies a part in a SQL statement which is currently in use.
data Feature = CSelect
             | CFrom
             | CWhere
             | CDistinct
             | COrderBy
             | CWindowFunction
             | CAggrAndGrouping
             deriving Enum

type FeatureSet = EnumSet Feature

-- | Lists all features which lead to a termination, for a given feature
-- coming from an operator placed below.
terminatingFeatures :: Feature -> [Feature]
terminatingFeatures bottomF = case bottomF of
    CSelect          -> []
    CFrom            -> []
    CWhere           -> []
    -- Distinction has to occur before:
    --
    --     * Projection of columns: Because there exist cases where to much gets
    --       removed.
    --
    --     * Aggregation: Because the previous result set influences the value
    --       of aggregate functions, including duplicates.
    --
    --     * Grouping: Grouping could project away columns, which are needed for
    --       duplicate elimination.
    --
    CDistinct        -> [CSelect, CAggrAndGrouping]
    -- The ORDER BY clause will only be used on top.
    COrderBy         -> []
    -- Problematic cases:
    --
    --     * Filtering: May change the intermediate result set.
    --
    --     * Duplicate removal with DISTINCT has another semantic meaning.
    --
    --     * Stacked window functions can possibly have other windows and window
    --       functions can not be nested.
    --
    --     * Window functions in ORDER BY clauses are executed row wise.
    --
    CWindowFunction  -> [CWhere, CDistinct, CAggrAndGrouping, COrderBy]
    -- Problematic cases:
    -- 
    --     * Filtering: May change intermediate result set.
    --
    --     * Is there a case, where OLAP functions using windows with aggregates
    --       makes sense? TODO otherwise the representation has to be changed,
    --       since aggregates over aggregates are not allowed. In fact
    --       AdvancedExpr has to be changed (AEBase, AEAggr, AEWindow) and two
    --       types has to be added AggrExpr/WindowExpr.
    --
    CAggrAndGrouping -> [CWhere, CWindowFunction, CAggrAndGrouping]

terminates :: [Feature] -> [Feature] -> Bool
terminates topFs bottomFs =
    any $ map (flip elem conflictingFs) topFs
  where
    conflictingFs = concatMap terminatingFeatures bottomFs

-- LitTable [Tuple] SchemaInfos    -> CFrom
-- LitTable [] SchemaInfos         -> CFrom, CWhere
-- 
-- TableRef SemInfTableRef         -> CFrom
-- 
-- RowNum SemInfRowNum             -> CSelect, CWindowFunction
-- RowRank SemInfRank              -> CSelect, CWindowFunction
-- Rank SemInfRank                 -> CSelect, CWindowFunction
-- Project [(AttrName, Expr)]      -> CSelect
-- Select Expr                     -> CWhere
-- Distinct ()                     -> CDistinct
-- Aggr SemInfAggr                 -> CSelect, CAggregateFunction, CGroupBy
-- 
-- Serialize (Maybe DescrCol, SerializeOrder, [PayloadCol])
--                                 -> []
-- 
-- 
-- Cross ()                        -> CFrom, CSelect
-- EqJoin SemInfEqJoin             -> CFrom, CSelect, CWhere
-- ThetaJoin SemInfJoin            -> CFrom, CSelect, CWhere
-- SemiJoin SemInfJoin             -> CWhere
-- AntiJoin SemInfJoin             -> CWhere
-- DisjUnion                       -> []
-- Difference                      -> []
