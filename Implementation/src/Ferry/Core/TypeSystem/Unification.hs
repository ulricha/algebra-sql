module Ferry.Core.TypeSystem.Unification where
    
import Ferry.Core.TypeSystem.Types
import Ferry.TypedCore.Data.Type
import Ferry.Compiler.Error.Error

import Control.Applicative hiding (Const(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Error

import qualified Data.List as L
import qualified Data.Set as S


unify :: FType -> FType -> AlgW ()
unify a  b = do
                a' <- applyS $ pure a
                b' <- applyS $ pure b
                unify' a' b'

unify' :: FType -> FType -> AlgW ()
unify' FInt        FInt        = pure ()
unify' FFloat      FFloat      = pure ()
unify' FBool       FBool       = pure ()
unify' FString     FString     = pure ()
unify' (FList a)   (FList b)   = unify a b
unify' (FFn a1 b1) (FFn a2 b2) = unify a1 a2 >> unify b1 b2
unify' (FRec r1)   (FRec r2)   = unifyRecords r1 r2
unify' t           v@(FVar a)  = if v == t || S.notMember a (ftv t)
                                     then updateSubstitution v t
                                     else pure ()
unify' v@(FVar a)  t           = if v == t || S.notMember a (ftv t)
                                     then updateSubstitution v t
                                     else pure ()
unify' a1          a2          = throwError $ UnificationError a1 a2

unifyRecords :: [(RLabel, FType)] -> [(RLabel, FType)] -> AlgW ()
unifyRecords ((l1, t1):r1) ((l2, t2):r2) = do
                                                unifyFields l1 l2
                                                unify t1 t2
                                                unifyRecords r1 r2
unifyRecords []         [] = pure ()
unifyRecords r1         r2 = throwError $ UnificationRecError r1 r2 

unifyFields :: RLabel -> RLabel -> AlgW ()
unifyFields r1@(RLabel l1) r2@(RLabel l2) = if l1 == l2 then return () else throwError $ UnificationOfRecordFieldsFailed r1 r2
unifyFields r1@(RVar i) r2                = if r1 == r2 || S.notMember i (frv r2)
                                              then updateRecSubstitution r1 r2
                                              else pure ()
unifyFields r1          r2@(RVar i)       = if r1 == r2 || S.notMember i (frv r1)
                                              then updateRecSubstitution r2 r1
                                              else pure ()
unifyFields r1          r2                = throwError $ UnificationFail r1 r2

mergeQuals :: [Pred] -> [Pred] -> AlgW [Pred]
mergeQuals t1     t2 = consistents $ mergeQualsW t1 t2
 where
    mergeQualsW []     t  = pure t
    mergeQualsW t      [] = pure t
    mergeQualsW (p:ps) t  = if L.elem p t then mergeQualsW ps t else mergeQualsW ps (p:t)

insertQual :: Pred -> [Pred] -> AlgW [Pred]
insertQual p@(IsIn _ _) ps = pure (p:ps)
insertQual p@(Has v f t) (p2@(Has v2 f2 t2):ps) | v == v2 && f == f2 = do
                                                                        unify t t2
                                                                        t' <- applySubst t
                                                                        pure ((Has v f t'):ps)
                                                | otherwise          = do
                                                                        ps' <- insertQual p ps
                                                                        pure (p2:ps')
insertQual p           (p':ps) = do
                                    ps' <- insertQual p ps
                                    pure (p':ps')
insertQual p           []      = pure [p]

mergeQuals' :: [[Pred]] -> AlgW [Pred]
mergeQuals' pss = foldr (\p r -> do
                                   r' <- r
                                   mergeQuals p r') (pure []) pss
                                   
consistents :: AlgW [Pred] -> AlgW [Pred]
consistents pss = do 
                       ps <- pss
                       case ps of
                        (p:ps) -> do
                                    p' <- consistent p
                                    applySubst ps
                                    ps' <- consistents $ pure ps
                                    applySubst (p':ps')
                        [] -> pure []

consistent :: Pred -> AlgW Pred
consistent p@(Has (FRec els) f t)  = case (L.lookup f els) of
                                       Just a -> do
                                                   unify a t
                                                   applySubst p
                                       Nothing -> pure p
consistent p                       = pure p
