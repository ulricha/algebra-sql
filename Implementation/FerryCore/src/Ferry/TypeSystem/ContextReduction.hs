module Ferry.TypeSystem.ContextReduction where
    
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.TypeClasses
import Ferry.TypedCore.Data.Instances()

import qualified Data.List as L
import qualified Data.Set as S

reduce :: Qual FType -> ClassEnv -> ([Pred], Qual FType)
reduce (preds :=> tau) _ = (gamPreds, tyPreds :=> tau)
    where
        (tyPreds, gamPreds) = L.partition hasQVar (simplifyPreds $ foldr filterImpossiblePreds [] recPr)
        (recPr, _classPr) = L.partition (\p -> case p of
                                                Has _ _ _ -> True
                                                _ -> False) preds

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds ps = foldr (\x l -> (flatten x) : l)  [] groups
   where
    flatten x = case x of
                 [x'] -> x'
                 _   -> error "Multiple types for one field"
    groups = map L.nub $ L.groupBy (\c1 c2 -> case (c1, c2) of
                                                ((Has f1 r1 _), (Has f2 r2 _)) -> f1 == f2 && r1 == r2
                                                _                              -> error "Wrong type of predicate") ps


filterImpossiblePreds :: Pred -> [Pred] -> [Pred]
filterImpossiblePreds p@(Has (FVar v) _ t) ps = case S.member v (ftv t) of
                                    True -> error "infinite type in record"
                                    False -> p:ps
filterImpossiblePreds p@(Has (FRec _) _ (FVar _)) ps = (p:ps)
                                                         
filterImpossiblePreds p@(Has (FRec rs) f t) ps = case L.lookup f rs of
                                       Nothing -> error "record does not contain file"
                                       (Just t2) -> if t == t2 then ps else error $ show p ++ "incompatable types"
filterImpossiblePreds _                    _ = error "Not a record type"