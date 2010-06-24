module Ferry.TypedCore.Convert.Specialize where

import Ferry.TypedCore.Convert.Traverse
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type

groupNSpecialize :: CoreExpr -> CoreExpr
groupNSpecialize = traverse f
    where
        f :: FoldCore CoreExpr Param RecElem
        f = idFoldCore {varF = fn}
        fn :: Qual FType -> String -> CoreExpr
        fn t s = case s of
                  "groupByN" -> case typeSize t of
                                    n | 0 < n && n <= 6 -> Var t $ "groupBy" ++ (show n)
                                      | otherwise       -> Var t "groupByN"
                  otherwise  -> (Var t s)
        typeSize :: Qual FType -> Int
        typeSize (_ :=> (FFn (FFn _ t2) _)) = case t2 of
                                               (FRec r) -> length r
                                               _        -> 0
{-
data FType where
    FGen :: Int -> FType
    FInt :: FType
    FFloat :: FType
    FString :: FType
    FBool :: FType
    FList :: FType -> FType
    FVar :: Ident -> FType
    FRec :: [(RLabel, FType)] -> FType
    FFn :: FType -> FType -> FType
    FTF :: FTFn -> FType -> FType
-}