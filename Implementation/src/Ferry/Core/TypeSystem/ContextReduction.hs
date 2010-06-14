module Ferry.Core.TypeSystem.ContextReduction where
    
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.TypeClasses
import Ferry.TypedCore.Data.Instances

import qualified Data.List as L
import qualified Data.Set as S

import System.IO.Unsafe

reduce :: Qual FType -> ClassEnv -> ([Pred], Qual FType)
reduce (preds :=> tau) cEnv = (gamPreds, tyPreds :=> tau)
    where
        (tyPreds, gamPreds) = L.partition hasQVar (simplifyPreds $ foldr filterImpossiblePreds [] recPr)
        (recPr, classPr) = L.partition (\p -> case p of
                                                Has _ _ _ -> True
                                                otherwise -> False) preds

{-
classExists :: Pred -> ClassEnv -> Pred
classExists p@(IsIn c t) env = if defined c env

classPreds :: Pred -> ClassEnv -> Pred
classPreds (IsIn c t) env = not isTrivial
  where
    isTrivial = L.member t $  map (\(_ :=> ty) -> ty) $ map snd $ M.lookup c env  
-}

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds ps = foldr (\x l -> (flatten x) : l)  [] groups
   where
    flatten x = case x of
                 [x] -> x
                 _   -> error "Multiple types for one field"
    groups = map L.nub $ L.groupBy (\(Has f1 r1 _) (Has f2 r2 _) -> f1 == f2 && r1 == r2) ps


filterImpossiblePreds :: Pred -> [Pred] -> [Pred]
filterImpossiblePreds p@(Has (FVar v) f t) ps = case S.member v (ftv t) of
                                    True -> error "infinite type in record"
                                    False -> p:ps
filterImpossiblePreds p@(Has (FRec rs) f (FVar v)) ps = (p:ps)
                                                         
filterImpossiblePreds p@(Has (FRec rs) f t) ps = case L.lookup f rs of
                                       Nothing -> error "record does not contain file"
                                       (Just t2) -> if t == t2 then ps else error $ show p ++ "incompatable types"
filterImpossiblePreds _                    _ = error "Not a record type"