-- | Datatypes and functions which determine termination of SQL fragements.
module Database.Algebra.SQL.Termination
    ( FeatureSet
    , terminatesOver
    , noneF
    , projectF
    , filterF
    , tableF
    , dupElimF
    , orderingF
    , windowFunctionF
    , aggrAndGroupingF
    , module Data.Monoid
    ) where

import           Data.Monoid
import qualified Data.Set    as S
import           Data.List   (intercalate)

-- | Specifies a part in a SQL statement which is currently in use.
data Feature = FProjection -- ^ Projection of columns.
             | FTable -- ^ Physical or virtual table.
             | FFilter -- ^ Filtering of rows.
             | FDupElim
             | FOrdering
             | FWindowFunction
             | FAggrAndGrouping
             deriving (Eq, Ord, Show)

-- TODO maybe use just list, since we usually have so few
newtype FeatureSet = F { unF :: S.Set Feature }

wrap :: Feature -> FeatureSet
wrap = F . S.singleton

noneF, filterF, tableF, dupElimF, orderingF, windowFunctionF, aggrAndGroupingF
    :: FeatureSet
noneF = F S.empty
projectF = wrap FProjection
filterF = wrap FFilter
tableF = wrap FTable
dupElimF = wrap FDupElim
orderingF = wrap FOrdering
windowFunctionF = wrap FWindowFunction
aggrAndGroupingF = wrap FAggrAndGrouping

instance Monoid FeatureSet where
    mempty              = noneF
    mappend (F l) (F r) = F $ l `S.union` r
    mconcat fs          = F $ S.unions $ map unF fs

instance Show FeatureSet where
    show (F s) = "<" ++ intercalate ", " (map show $ S.toList s) ++ ">"


-- | Lists all features which lead to a termination, for a given feature
-- coming from an operator placed below.
terminatingFeatures :: Feature -> FeatureSet
terminatingFeatures bottomF = F $ case bottomF of
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
terminatesOver :: FeatureSet -> FeatureSet -> Bool
terminatesOver (F topFs) (F bottomFs) =
    any (`S.member` conflictingFs) $ S.toList topFs
  where
    (F conflictingFs) = mconcat $ map terminatingFeatures $ S.toList bottomFs

