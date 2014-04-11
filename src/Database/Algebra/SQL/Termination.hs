-- | Datatypes and functions which determine termination of SQL fragements.
module Database.Algebra.SQL.Termination
    ( Feature(..)
    , FeatureSet
    , terminates
    ) where

import qualified Data.Set as S

-- TODO module needs Set, maybe hide somehow
-- TODO EnumSet would not require Eq and Ord, and maybe faster (IntSet wrapper),
-- but requires Enum instead

-- | Specifies a part in a SQL statement which is currently in use.
data Feature = FProjection -- ^ Projection of columns.
             | FTable -- ^ Physical or virtual table.
             | FFilter -- ^ Filtering of rows.
             | FDupElim
             | FOrdering
             | FWindowFunction
             | FAggrAndGrouping
             deriving (Eq, Ord, Show)

type FeatureSet = S.Set Feature

-- | Lists all features which lead to a termination, for a given feature
-- coming from an operator placed below.
terminatingFeatures :: Feature -> FeatureSet
terminatingFeatures bottomF = case bottomF of
    FProjection      -> S.empty
    FTable           -> S.empty
    FFilter          -> S.empty
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
    FDupElim         -> S.fromList [FProjection, FAggrAndGrouping]
    -- The ORDER BY clause will only be used on top.
    FOrdering        -> S.empty
    -- Problematic cases:
    --
    --     * Filtering: May change the intermediate result set.
    --
    --     * Duplicate removal with DISTINCT has another semantic meaning.
    --
    --     * Stacked window functions can possibly have other windows and window
    --       functions can not be nested.
    --
    --     * Aggregates of window functions can not be built.
    --
    FWindowFunction  ->
        S.fromList [FFilter, FDupElim, FWindowFunction, FAggrAndGrouping]
    -- Problematic cases:
    -- 
    --     * Filtering: May change intermediate result set.
    --
    --     * Aggregate functions can not be stacked.
    --
    --     * Is there a case, where OLAP functions using windows with aggregates
    --       makes sense? It is possible, and inlining works, therefore it is
    --       enabled.
    --
    FAggrAndGrouping -> S.fromList [FFilter, FAggrAndGrouping]

-- | Determines whether two feature sets collide and therefore whether we should
-- terminate a SQL fragment.
terminates :: FeatureSet -> FeatureSet -> Bool
terminates topFs bottomFs =
    any (flip S.member conflictingFs) $ S.toList topFs
  where
    conflictingFs = S.unions $ map terminatingFeatures $ S.toList bottomFs

