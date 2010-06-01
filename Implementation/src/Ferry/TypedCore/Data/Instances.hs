{-# LANGUAGE TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Instances where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.Substitution
import Ferry.TypedCore.Data.TypedCore

import qualified Data.Set as S
import qualified Data.Map as M

instance Substitutable FType where
  apply' b s (FList t)             = FList $ apply' b s t 
  apply' b s (FFn t1 t2)           = FFn (apply' b s t1) (apply' b s t2)
  apply' b s (FRec rs)             = FRec $ S.map (\(n, t) -> (n, apply' b s t)) rs
  apply' b s v@(FVar i) = case S.member i b || M.notMember i s of
                                        True -> v
                                        False -> s M.! i
  apply' _ _    t                  = t -- If the substitution is not applied to a container type or variable just stop primitives cannot be substituted

instance Substitutable QualTy where
  apply' b s (FType a)   = FType $ apply' b s a
  apply' b s (Qual n v t) | S.notMember v b = case M.lookup v s of
                                                Nothing -> Qual n v $ apply' b s t
                                                Just (FVar a) -> Qual n a $ apply' b s t
                                                Just _       -> Qual n v $ apply' b s t
                          | otherwise       = Qual n v $ apply' b s t
                          
instance Substitutable TyScheme where
  apply' b s (Forall i t) = Forall i $ apply' (S.insert i b) s t
  apply' b s (QualTy t) = QualTy $ apply' b s t
    
instance Substitutable TyEnv where
  apply' b s m = M.map (apply' b s) m
  
instance Substitutable CoreExpr where
  apply' b s (BinOp t o c1 c2) = BinOp (apply' b s t) o (apply' b s c1) (apply' b s c2)
  apply' b s (UnaOp t o c)      = UnaOp (apply' b s t) o (apply' b s c)
  apply' b s (Constant t c)    = Constant (apply' b s t) c
  apply' b s (Var t x)         = Var (apply' b s t) x
  apply' b s (App t c a)       = App (apply' b s t) (apply' b s c) (apply' b s a)
  apply' b s (Let t x c1 c2)   = Let (apply' b s t) x (apply' b s c1) (apply' b s c2)
  apply' b s (Rec t es)        = Rec (apply' b s t) $ map (apply' b s) es
  apply' b s (Cons t c1 c2)    = Cons (apply' b s t) (apply' b s c1) (apply' b s c2)
  apply' b s (Nil t)           = Nil (apply' b s t)
  apply' b s (Elem t c f)      = Elem (apply' b s t) (apply' b s c) f
  apply' b s (Table t n c k)   = Table (apply' b s t) n c k
  apply' b s (If t c1 c2 c3)   = If (apply' b s t) (apply' b s c1) (apply' b s c2) (apply' b s c3)

instance Substitutable Param where
    apply' b s (ParExpr t c) = ParExpr (apply' b s t) (apply' b s c)
    apply' b s (ParAbstr t pa c) = ParAbstr (apply' b s t) pa (apply' b s c)
        
instance Substitutable RecElem where
    apply' b s (RecElem t x c) = RecElem (apply' b s t) x (apply' b s c)

{- | Instances of VarContainer class-}
  
instance VarContainer FType where
  ftv (FVar a)    = S.singleton a
  ftv (FList t)   = ftv t
  ftv (FRec s)    = S.unions $ map (ftv . snd) $ S.toList s
  ftv (FFn t1 t2) = ftv t1 `S.union` ftv t2
  ftv _           = S.empty

instance VarContainer TyScheme where
  ftv (Forall i t) = S.delete i $ ftv t 
  ftv (QualTy t)    = ftv t

instance VarContainer QualTy where
  ftv (Qual _ v t) = S.singleton v `S.union` (ftv t)
  ftv (FType t)    = ftv t
  
instance VarContainer TyEnv where
  ftv m = S.unions $ M.elems $ M.map ftv m
  
instance HasType CoreExpr where
  typeOf (BinOp t o c1 c2) = t
  typeOf (UnaOp t o c)     = t
  typeOf (Constant t c)    = t
  typeOf (Var t x)         = t
  typeOf (App t c a)       = t
  typeOf (Let t x c1 c2)   = t
  typeOf (Rec t es)        = t
  typeOf (Cons t c1 c2)    = t
  typeOf (Nil t)           = t
  typeOf (Elem t c f)      = t
  typeOf (Table t n c k)   = t
  typeOf (If t c1 c2 c3)   = t