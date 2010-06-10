module Ferry.Core.TypeSystem.AlgorithmW (typeInfer) where

import Ferry.Core.TypeSystem.Types    
import qualified Ferry.Core.Data.Core as C
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.Substitution 
import Ferry.TypedCore.Data.Base
import Ferry.Compiler.Error.Error
import Ferry.TypedCore.Data.Instances
import Ferry.Front.Data.Base hiding (VarContainer)
import Ferry.Core.TypeSystem.Unification
import Ferry.Core.TypeSystem.ContextReduction


import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Control.Applicative hiding (Const(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

typeInfer :: TyEnv -> C.CoreExpr -> (Either FerryError CoreExpr, Subst)
typeInfer gam c = runAlgW gam $ algW c
                  
algW :: C.CoreExpr -> AlgW CoreExpr
algW (C.Constant c)  = Constant <$> typeOfConst c <*> pure c
algW (C.Var x)       = Var <$> (inst $ lookupVariable x) <*> pure x
algW (C.Let x c1 c2) = applyS $ Let <$> liftM typeOf c2' <*> pure x <*> c1' <*> c2' 
 where
     c1' = algW c1
     c2' = do
            (q :=> t) <- liftM typeOf c1'
            addToEnv x (Forall 0 $ q :=> t) (algW c2)
algW (C.Nil) = Nil <$> liftM (\v -> [] :=> FVar v) freshTyVar
algW (C.Cons c1 c2) = do
                        c1' <- algW c1
                        c2' <- algW c2
                        let (q1 :=> t1) = typeOf c1'
                        let (q2 :=> t2) = typeOf c2'
                        unify t2 $ FList t1
                        applySubst $ Cons ((mergeQuals q1 q2) :=> t2) c1' c2'
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
                         s'' <- getSubst
                         applySubst $ If (mergeQuals' [q1, q2, q3] :=> t2) c1' c2' c3'
algW (C.Table n cs ks) = let recTys = L.sortBy (\(n1, t1) (n2, t2) -> compare n1 n2) $ map columnToRecElem cs
                             in if length (uniqueKeys recTys) == length recTys 
                                 then applySubst $ Table ([] :=> FRec recTys) n (map columnToTyColumn cs) (map keyToTyKey ks)
                                 else throwError $ RecordDuplicateFields (Just n) $ map columnToRecElem cs
algW (C.Elem e i) = do
                       a <- liftM FVar freshTyVar
                       c1' <- algW e
                       let (q1 :=> t1) = typeOf c1'
                       case t1 of
                            (FVar i) -> applySubst $ Elem ((mergeQuals q1 [Has (FVar i) i a]) :=> a) c1' i
                            (FRec els) -> case lookup i els of
                                            Nothing -> throwError $ RecordWithoutI t1 i
                                            (Just a) -> applySubst $ Elem (q1 :=> a) c1' i
                            _       -> throwError $ NotARecordType t1
algW (C.Rec elems) = do
                        els <- recElemsToTyRecElems elems
                        let (qs, nt) = foldr (\(RecElem (q :=> t) n _) (qs, nt) -> (q:qs, (n, t):nt)) ([], []) els
                            q = mergeQuals' qs
                            t = FRec $ L.sortBy (\(n1, t1) (n2, t2) -> compare n1 n2) nt
                         in if (length (uniqueKeys nt) == length nt) 
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
                            applySubst $ BinOp (mergeQuals' [q, q1, q2] :=> otr) (Op o) e1' e2'
algW (C.UnaOp (C.Op o) e1) = 
                          do
                            ot <- inst $ lookupVariable o
                            let (q :=> FFn ot1 otr) = ot
                            e1' <- algW e1
                            let (q1 :=> t1) = typeOf e1'
                            unify t1 ot1
                            applySubst $ UnaOp (mergeQuals q q1 :=> otr) (Op o) e1'
algW (C.App e arg) = do
                         ar <- liftM FVar freshTyVar
                         e' <- algW e
                         arg' <- algWArg arg
                         let (qt1 :=> t1) = typeOf e'
                             (qta :=> ta) = typeOf arg'
                         unify t1 (FFn ta ar)
                         let rqt = mergeQuals qt1 qta
                         t <- applyS $ pure (rqt :=> ta)
                         applySubst $ App t e' arg'
                         

algWArg :: C.Param -> AlgW Param
algWArg (C.ParExpr e) = do
                         e' <- algW e
                         applySubst $ ParExpr (typeOf e') e'  
algWArg (C.ParAbstr p e) = do
                             let vars = getVars p
                             bindings <- foldr (\v r -> do
                                                          t <- liftM (\v -> [] :=> FVar v) freshTyVar
                                                          r' <- r
                                                          return $ (v, t):r' 
                                                          ) (pure []) vars
                             e' <- foldr (\(v, t) r -> addToEnv v (Forall 0 t) r) (algW e) bindings
                             let (q :=> rt) = typeOf e'
                             let t = q :=> (foldr (\(_, _ :=> t) r -> FFn t r) rt bindings)  
                             applySubst $ ParAbstr t (toPattern vars) e'
                             
toPattern :: [String] -> Pattern
toPattern [x]   = PVar x
toPattern xs    = Pattern xs
                    
getVars :: C.Pattern -> [String]
getVars (C.PVar v) = [v]
getVars (C.Pattern p) = p                     
                           
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

columnToRecElem :: C.Column -> (String, FType)
columnToRecElem (C.Column s t) = (s, typeToFType t)

typeToFType :: C.Type -> FType
typeToFType C.TInt = FInt
typeToFType C.TFloat = FFloat
typeToFType C.TString = FString
typeToFType C.TBool = FBool   

keyToTyKey :: C.Key -> Key
keyToTyKey (C.Key k) = Key k                     

typeOfConst :: Const -> AlgW (Qual FType)
typeOfConst (CInt _) = pure $ [] :=> FInt
typeOfConst (CFloat _) = pure $ [] :=> FFloat
typeOfConst (CBool _) = pure $ [] :=>  FBool
typeOfConst (CString _) = pure $ [] :=> FString


gen :: AlgW (Qual FType) -> AlgW ([Pred], TyScheme)
gen s = do
           s' <- s
           gam <- getGamma
           let freeInT = ftv s'
           let freeInGam = ftv gam
           let quant = S.toList $ freeInT S.\\ freeInGam
           let substs = zip quant [FGen i | i <- [1..]]
           qualT <- foldr (\(i, q) -> localAddSubstitution (FVar i) q) (applyS $ pure s') substs
           let (qg, qt) = reduce qualT M.empty
           return $ (qg, Forall (length substs) $ qt)
           
inst :: AlgW TyScheme -> AlgW (Qual FType)
inst s = do
            s' <- s
            subst <- getSubst
            case s' of
                Forall 0 t -> applyS $ pure t
                Forall i t -> do
                                freshVar <- freshTyVar
                                localAddSubstitution (FGen i) (FVar freshVar) (inst $ pure $ Forall (i-1) t)
