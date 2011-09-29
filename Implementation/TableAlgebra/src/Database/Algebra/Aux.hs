module Database.Algebra.Aux where

import qualified Data.Map as M

lookupUnsafe :: (Ord k, Show k) => M.Map k a -> String -> k -> a
lookupUnsafe m s u = 
    case M.lookup u m of
        Just p -> p
        Nothing -> error $ s ++ " " ++ (show u)
