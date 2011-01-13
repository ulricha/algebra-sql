{-| Infer types for a program, transform an untyped core AST into a typed core AST.
Standard Algorithm W, with some modifications to deal with records.-}
module Ferry.TypeSystem.AlgorithmW (typeInfer) where

import Ferry.TypeSystem.Types    
import qualified Ferry.Core.Data.Core as C
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.Substitution 
import Ferry.Compiler.Error.Error
import Ferry.Common.Data.Base (Const (..)) 
import Ferry.TypeSystem.Unification
import Ferry.TypeSystem.ContextReduction


import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Control.Applicative hiding (Const(..))
import Control.Monad.Reader
import Control.Monad.Error

typeInfer :: TyEnv -> C.CoreExpr -> (Either FerryError CoreExpr, Subst)
typeInfer gam c = runAlgW gam $ do 
                                   e <- algW c
                                   -- (p, s@(Forall _ t)) <- gen $ pure $ typeOf e
                                   let (q :=> t) = typeOf e
                                   q' <- consistents $ pure q
                                   let (qs, q'' :=> _t') = reduce (q' :=> t) M.empty
                                   qual <- mergeQuals qs q''
                                   applySubst $ setType (qual :=> t) e
                                   --pure e
                  
algW :: C.CoreExpr -> AlgW CoreExpr
algW (C.Constant c)  = Constant <$> typeOfConst c <*> pure c
algW (C.Var x)       = Var <$> (inst $ lookupVariable x) <*> pure x
algW (C.Let x c1 c2) = do
                            c1' <- algW c1
                            (p, ts@(Forall _ _ qt)) <- gen $ pure $ typeOf c1'
                            let c1'' = setType qt  c1'
                            c2' <- addToEnv x ts (algW c2)
                            let (q2 :=> t2) = typeOf c2'
                            q <- mergeQuals q2 p
                            applySubst $ Let (q :=> t2) x c1'' c2' 
algW (C.Nil) = Nil <$> liftM (\v -> [] :=> (list $ FVar v)) freshTyVar
algW (C.Cons c1 c2) = do
                        c1' <- algW c1
                        c2' <- algW c2
                        let (q1 :=> t1) = typeOf c1'
                        let (q2 :=> t2) = typeOf c2'
                        unify t2 $ FList t1
                        q <- mergeQuals q1 q2
                        applySubst $ Cons (q :=> t2) c1' c2'
algW (C.If c1 c2 c3) = do
                         c1' <- algW c1
                         c2' <- algW c2
                         c3' <- algW c3
                         let (q1 :=> t1) = typeOf c1'
                         let (q2 :=> t2) = typeOf c2'
                         let (q3 :=> t3) = typeOf c3' 
                         s <- getSubst
                         unify (apply s $ t1) FBool
                         s' <- getSubst
                         unify (apply s' $ t2) (apply s' $ t3) 
                         q <- mergeQuals' [q1, q2, q3]
                         applySubst $ If (q :=> t2) c1' c2' c3'
algW (C.Table n cs ks) = let recTys = L.sortBy (\(n1, _t1) (n2, _t2) -> compare n1 n2) $ map columnToRecElem cs
                             in if length (uniqueKeys recTys) == length recTys 
                                 then applySubst $ Table ([] :=> (list $ FRec recTys)) n (map columnToTyColumn cs) (map keyToTyKey ks)
                                 else throwError $ RecordDuplicateFields (Just n) $ map columnToRecElem cs
algW (C.Elem e i) = do
                       fresh <- liftM FVar freshTyVar
                       c1' <- algW e
                       let (q1 :=> t1) = typeOf c1'
                       case t1 of
                            (FVar _v) -> do 
                                          q <- insertQual (Has t1 (RLabel i) fresh) q1 
                                          applySubst $ Elem (q :=> fresh) c1' i
                            (FRec els) -> case lookup (RLabel i) els of
                                            Nothing -> throwError $ RecordWithoutI t1 i
                                            (Just a) -> applySubst $ Elem (q1 :=> a) c1' i
                            _       -> throwError $ NotARecordType t1
algW (C.Rec elems) = do
                        els <- recElemsToTyRecElems elems
                        let (qs, nt) = foldr (\(RecElem (q :=> t) n _) (qs', nt') -> (q:qs', (RLabel n, t):nt')) ([], []) els
                        let t = FRec $ L.sortBy (\(n1, _t1) (n2, _t2) -> compare n1 n2) nt
                        q <- mergeQuals' qs
                        if (length (uniqueKeys nt) == length nt) 
                                then applySubst $ Rec (q :=> t) els
                                else throwError $ RecordDuplicateFields Nothing nt
algW (C.BinOp (C.Op o) e1 e2) = do
                            ot <- inst $ lookupVariable o
                            let (q :=> FFn ot1 (FFn ot2 otr)) = ot
                            e1' <- algW e1
                            e2' <- algW e2
                            let (q1 :=> t1) = typeOf e1'
                                (q2 :=> t2) = typeOf e2'
                            unify t1 ot1
                            unify t2 ot2
                            q' <- mergeQuals' [q, q1, q2] 
                            applySubst $ BinOp (q' :=> otr) (Op o) e1' e2'
{- algW (C.UnaOp (C.Op o) e1) = 
                          do
                            ot <- inst $ lookupVariable o
                            let (q :=> FFn ot1 otr) = ot
                            e1' <- algW e1
                            let (q1 :=> t1) = typeOf e1'
                            unify t1 ot1
                            q' <- mergeQuals q q1 
                            applySubst $ UnaOp (q :=> otr) (Op o) e1' -}
algW (C.App e arg) = do
                         ar <- liftM FVar freshTyVar
                         e' <- algW e
                         arg' <- algWArg arg
                         let (qt1 :=> t1) = typeOf e'
                             (qta :=> ta) = typeOf arg'
                         unify t1 (FFn ta ar)
                         q1 <- applySubst qt1
                         q2 <- applySubst qta
                         
                         rqt <- (mergeQuals q1 q2)
                         t <- applyS $ pure (rqt :=> ar)
                         applySubst $ App t e' arg'
                         

algWArg :: C.Param -> AlgW Param
algWArg (C.ParExpr e) = do
                         e' <- algW e
                         applySubst $ ParExpr (typeOf e') e'  
algWArg (C.ParAbstr p e) = do
                             let vars' = p
                             bindings <- foldr (\v r -> do
                                                          t <- liftM (\var' -> [] :=> FVar var') freshTyVar
                                                          r' <- r
                                                          return $ (v, t):r' 
                                                          ) (pure []) vars'
                             e' <- foldr (\(v, t) r -> addToEnv v (Forall 0 0 t) r) (algW e) bindings
                             let (q :=> rt) = typeOf e'
                             let t = q :=> (foldr (\(_, _ :=> ty) r -> FFn ty r) rt bindings)  
                             applySubst $ ParAbstr t vars' e'
                             
{-
toPattern :: [String] -> Pattern
toPattern [x]   = PVar x
toPattern xs    = Pattern xs
                    
getVars :: C.Pattern -> [String]
getVars (C.PVar v) = [v]
getVars (C.Pattern p) = p                     
-}                           
uniqueKeys :: Eq k => [(k, a)] -> [(k, a)]
uniqueKeys l1 = L.nubBy (\(k1, _) (k2, _) -> k1 == k2) l1  

recElemsToTyRecElems :: [C.RecElem] -> AlgW [RecElem]
recElemsToTyRecElems (x:xs) = do
                                x' <- recElemToTyRecElem x
                                xs' <- recElemsToTyRecElems xs
                                pure (x':xs')
recElemsToTyRecElems [] = pure []

recElemToTyRecElem :: C.RecElem -> AlgW RecElem
recElemToTyRecElem (C.RecElem s e) = do
                                        e' <- algW e
                                        let t = typeOf e'
                                        applySubst $ RecElem t s e'    
                           
                            
columnToTyColumn :: C.Column -> Column
columnToTyColumn (C.Column s t) = Column s $ typeToFType t

columnToRecElem :: C.Column -> (RLabel, FType)
columnToRecElem (C.Column s t) = (RLabel s, typeToFType t)

typeToFType :: C.Type -> FType
typeToFType C.TInt = FInt
typeToFType C.TFloat = FFloat
typeToFType C.TString = FString
typeToFType C.TBool = FBool
typeToFType C.TUnit = FUnit   

keyToTyKey :: C.Key -> Key
keyToTyKey (C.Key k) = Key k                     

typeOfConst :: Const -> AlgW (Qual FType)
typeOfConst (CInt _) = pure $ [] :=> FInt
typeOfConst (CFloat _) = pure $ [] :=> FFloat
typeOfConst (CBool _) = pure $ [] :=>  FBool
typeOfConst (CString _) = pure $ [] :=> FString
typeOfConst (CUnit) = pure $ [] :=> FUnit


gen :: AlgW (Qual FType) -> AlgW ([Pred], TyScheme)
gen s = do
           s' <- s
           gam <- getGamma
           let freeInT = ftv s'
           let freeInGam = ftv gam
           let freeRInT = frv s'
           let freeRInGam = frv gam
           let quant = S.toList $ freeInT S.\\ freeInGam
           let quantR = S.toList $ freeRInT S.\\ freeRInGam
           let substs = zip quant [FGen i | i <- [1..]]
           let substsR = zip quantR [RGen i | i <- [1..]]
           qualT <- foldr (\(i, q) -> localAddSubstitution (FVar i) q) (applyS $ pure s') substs
           qualR <- foldr (\(i, q) -> localAddRecSubstitution (RVar i) q) (applyS $ pure qualT) substsR
           let (qg, qt) = reduce qualR M.empty
           return $ (qg, Forall (length substs) (length substsR) $ qt)
           
inst :: AlgW TyScheme -> AlgW (Qual FType)
inst s = do
            s' <- s
            case s' of
                Forall 0 0 t -> applyS $ pure t
                Forall 0 t ty -> do
                                  freshVar <- freshTyVar
                                  localAddRecSubstitution (RGen t) (RVar freshVar) (inst $ pure $ Forall 0 (t - 1) ty)
                Forall i t ty -> do
                                  freshVar <- freshTyVar
                                  localAddSubstitution (FGen i) (FVar freshVar) (inst $ pure $ Forall (i-1) t ty)
