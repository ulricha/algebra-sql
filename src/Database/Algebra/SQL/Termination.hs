-- | Datatypes and functions which determine termination of SQL fragements.
module Database.Algebra.SQL.Termination
    ( Featrue
    , terminates
    ) where

import Data.Set

-- | Specifies a part in a SQL statement which is currently in use.
data Feature = FProjection -- ^ Projection of columns.
             | FTable -- ^ Physical or virtual table.
             | FFilter -- ^ Filtering of rows.
             | FDupElim
             | FOrdering
             | FWindowFunction
             | FAggrAndGrouping
             deriving Ord

type FeatureSet = Set Feature

-- | Lists all features which lead to a termination, for a given feature
-- coming from an operator placed below.
terminatingFeatures :: Feature -> FeatureSet
terminatingFeatures bottomF = case bottomF of
    FProjection      -> empty
    FTable           -> empty
    FFilter          -> empty
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
    FDupElim         -> fromList [FProjection, FAggrAndGrouping]
    -- The ORDER BY clause will only be used on top.
    FOrdering        -> empty
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
    FWindowFunction  ->
        fromList [FFilter, FDupElim, FAggrAndGrouping, FOrdering]
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
    FAggrAndGrouping -> fromList [FFilter, FWindowFunction, FAggrAndGrouping]

terminates :: FeatureSet -> FeatureSet -> Bool
terminates topFs bottomFs =
    any (flip member conflictingFs) topFs
  where
    conflictingFs = unions $ map terminatingFeatures $ toList bottomFs

-- LitTable [Tuple] SchemaInfos    -> FTable
-- LitTable [] SchemaInfos         -> FTable, FFilter
-- 
-- TableRef SemInfTableRef         -> FTable
-- 
-- RowNum SemInfRowNum             -> FProjection, FWindowFunction
-- RowRank SemInfRank              -> FProjection, FWindowFunction
-- Rank SemInfRank                 -> FProjection, FWindowFunction
-- Project [(AttrName, Expr)]      -> FProjection
-- Select Expr                     -> FFilter
-- Distinct ()                     -> FDupElim
-- Aggr SemInfAggr                 -> FProjection, CAggregateFunction, CGroupBy
-- 
-- Serialize (Maybe DescrCol, SerializeOrder, [PayloadCol])
--                                 -> []
-- 
-- 
-- Cross ()                        -> FTable, FProjection
-- EqJoin SemInfEqJoin             -> FTable, FProjection, FFilter
-- ThetaJoin SemInfJoin            -> FTable, FProjection, FFilter
-- SemiJoin SemInfJoin             -> FFilter
-- AntiJoin SemInfJoin             -> FFilter
-- DisjUnion                       -> []
-- Difference                      -> []
