-- | This module provides some very general helper functions.
module Database.Algebra.Aux where

import qualified Data.IntMap as IM
import qualified Data.Map    as M

-- | Perform a map lookup and fail with the given error string if the key
-- is not present
lookupUnsafe :: Show a => IM.IntMap a -> String -> Int -> a
lookupUnsafe m s u =
    case IM.lookup u m of
        Just p -> p
        Nothing -> error $ s ++ " " ++ (show u) ++ " in " ++ (show m)

replace :: Eq a => a -> a -> a -> a
replace orig new x = if x == orig then new else x

reverseToIntMap :: M.Map a Int -> IM.IntMap a
reverseToIntMap m = IM.fromList $ map (\(a, b) -> (b, a)) $ M.toList m
                
reverseMap :: Ord b => M.Map a b -> M.Map b a
reverseMap m = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList m


