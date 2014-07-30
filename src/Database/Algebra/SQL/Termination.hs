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
data Feature = ProjectionF -- ^ Projection of columns.
             | TableF -- ^ Physical or virtual table.
             | FilterF -- ^ Filtering of rows.
             | DupElimF
             | OrderingF
             | WindowFunctionF
             | AggrAndGroupingF
             deriving (Eq, Ord, Show)

-- TODO maybe use just list, since we usually have so few
newtype FeatureSet = F { unF :: S.Set Feature }

wrap :: Feature -> FeatureSet
wrap = F . S.singleton

noneF, projectF, filterF, tableF, dupElimF, orderingF, windowFunctionF, aggrAndGroupingF :: FeatureSet
noneF = F S.empty
projectF = wrap ProjectionF
filterF = wrap FilterF
tableF = wrap TableF
dupElimF = wrap DupElimF
orderingF = wrap OrderingF
windowFunctionF = wrap WindowFunctionF
aggrAndGroupingF = wrap AggrAndGroupingF

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
    ProjectionF      -> S.empty
    TableF           -> S.empty
    FilterF          -> S.empty
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
    DupElimF         -> S.fromList [ProjectionF, AggrAndGroupingF]
    -- The ORDER BY clause will only be used on top.
    OrderingF        -> S.empty
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
    WindowFunctionF  ->
        S.fromList [FilterF, DupElimF, WindowFunctionF, AggrAndGroupingF]
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
    AggrAndGroupingF -> S.fromList [FilterF, AggrAndGroupingF]

-- | Determines whether two feature sets collide and therefore whether we should
-- terminate a SQL fragment.
terminatesOver :: FeatureSet -> FeatureSet -> Bool
terminatesOver (F topFs) (F bottomFs) =
    any (`S.member` conflictingFs) $ S.toList topFs
  where
    (F conflictingFs) = mconcat $ map terminatingFeatures $ S.toList bottomFs

