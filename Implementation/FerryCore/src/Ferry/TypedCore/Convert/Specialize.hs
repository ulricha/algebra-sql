{-# LANGUAGE TemplateHaskell #-}
{-| Provides a function that can replace groupByN occurences by a more specific one-}
module Ferry.TypedCore.Convert.Specialize where

import Ferry.TypedCore.Convert.Traverse
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.Impossible

groupNSpecialize :: CoreExpr -> CoreExpr
groupNSpecialize = traverse f
    where
        f :: FoldCore CoreExpr Param RecElem
        f = idFoldCore {varF = fnS}
        fnS :: Qual FType -> String -> CoreExpr
        fnS t s = case s of
                   "groupByN" -> case typeSize t of
                                     n | 0 < n && n <= 6 -> Var t $ "groupBy" ++ (show n)
                                       | otherwise       -> Var t "groupByN"
                   _  -> (Var t s)
        typeSize :: Qual FType -> Int
        typeSize (_ :=> (FFn (FFn _ t2) _)) = case t2 of
                                               (FRec r) -> length r
                                               _        -> 0
        typeSize _ = $impossible
