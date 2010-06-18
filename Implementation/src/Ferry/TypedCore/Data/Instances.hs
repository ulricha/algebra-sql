{-# LANGUAGE TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Instances where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.Substitution
import Ferry.TypedCore.Data.TypedCore

import qualified Data.Set as S
import qualified Data.Map as M

instance Substitutable FType where
  apply s (FList t)             = FList $ apply s t 
  apply s (FFn t1 t2)           = FFn (apply s t1) (apply s t2)
  apply s (FRec rs)             = FRec $ map (\(n, t) -> (n, apply s t)) rs
  apply s@(t, _) v@(FVar i) = case M.notMember v t of
                                    True -> v
                                    False -> t M.! v
  apply s@(t, _) v@(FGen i) = case M.notMember v t of
                                True -> v
                                False -> t M.! v
  apply _    t                  = t -- If the substitution is not applied to a container type or variable just stop primitives cannot be substituted

instance Substitutable t => Substitutable (Qual t) where
  apply s (preds:=> t) = (map (apply s) preds) :=> apply s t
  
instance Substitutable Pred where
  apply s (IsIn c t) = IsIn c $ apply s t
  apply s (Has r n t) = Has (apply s r) n (apply s t)  
                          
instance Substitutable TyScheme where
  apply s (Forall i r t) = Forall i r $ apply s t
    
instance Substitutable TyEnv where
  apply s m = M.map (apply s) m
  
instance Substitutable a => Substitutable [a] where
  apply s m = map (apply s) m
  
instance Substitutable CoreExpr where
  apply s (BinOp t o c1 c2) = BinOp (apply s t) o (apply s c1) (apply s c2)
  apply s (UnaOp t o c)      = UnaOp (apply s t) o (apply s c)
  apply s (Constant t c)    = Constant (apply s t) c
  apply s (Var t x)         = Var (apply s t) x
  apply s (App t c a)       = App (apply s t) (apply s c) (apply s a)
  apply s (Let t x c1 c2)   = Let (apply s t) x (apply s c1) (apply s c2)
  apply s (Rec t es)        = Rec (apply s t) $ map (apply s) es
  apply s (Cons t c1 c2)    = Cons (apply s t) (apply s c1) (apply s c2)
  apply s (Nil t)           = Nil (apply s t)
  apply s (Elem t c f)      = Elem (apply s t) (apply s c) f
  apply s (Table t n c k)   = Table (apply s t) n c k
  apply s (If t c1 c2 c3)   = If (apply s t) (apply s c1) (apply s c2) (apply s c3)

instance Substitutable Param where
    apply s (ParExpr t c) = ParExpr (apply s t) (apply s c)
    apply s (ParAbstr t pa c) = ParAbstr (apply s t) pa (apply s c)
        
instance Substitutable RecElem where
    apply s (RecElem t x c) = RecElem (apply s t) x (apply s c)

instance Substitutable RLabel where
    apply s@(_, r) v = case M.notMember v r of
                                      True -> v
                                      False -> r M.! v
    
{- | Instances of VarContainer class-}
  
instance VarContainer FType where
  ftv (FVar a)    = S.singleton a
  ftv (FList t)   = ftv t
  ftv (FRec s)    = S.unions $ map (ftv . snd) s
  ftv (FFn t1 t2) = ftv t1 `S.union` ftv t2
  ftv _           = S.empty
  frv (FList t)   = frv t
  frv (FRec s)    = S.unions $ map (\(r,t) -> S.union (frv r) (frv t)) s
  frv (FFn t1 t2) = frv t1 `S.union` frv t2
  frv _           = S.empty 
  hasQVar (FList t) = hasQVar t
  hasQVar (FRec s)  = and $ map (hasQVar . snd) s
  hasQVar (FFn t1 t2) = hasQVar t1 && hasQVar t2
  hasQVar (FGen _) = True
  hasQVar _        = False
  
instance VarContainer TyScheme where
  ftv (Forall i r t)  = ftv t 
  frv (Forall i r t)  = frv t
  hasQVar (Forall i r t) = if i > 0 then True else False

instance VarContainer t => VarContainer (Qual t) where
  ftv (preds :=> t) = S.unions $ (ftv t):(map ftv preds)
  frv (preds :=> t) = S.unions $ (frv t):(map frv preds)
  hasQVar (preds :=> t) = (&&) (hasQVar t) $ and $ map hasQVar preds 

instance VarContainer Pred where
  ftv (IsIn c t) = ftv t
  ftv (Has t _ t2) = ftv t `S.union` ftv t2
  frv (IsIn c t) = frv t
  frv (Has t _ t2) = frv t `S.union` frv t2
  hasQVar (IsIn _ t) = hasQVar t
  hasQVar (Has t _ t2) = hasQVar t && hasQVar t2

instance VarContainer TyEnv where
  ftv m = S.unions $ M.elems $ M.map ftv m
  frv m = S.unions $ M.elems $ M.map frv m
  hasQVar m = and $ map (hasQVar . snd) $ M.assocs m
  
instance VarContainer RLabel where
  ftv _ = S.empty
  frv (RVar i) = S.singleton i
  frv _        = S.empty
  hasQVar (RGen i) = True
  hasQVar _        = False
  
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
  setType t (BinOp _ o c1 c2) = BinOp t o c1 c2
  setType t (UnaOp _ o c)     = UnaOp t o c
  setType t (Constant _ c)    = Constant t c
  setType t (Var _ x)         = Var t x    
  setType t (App _ c a)       = App t c a  
  setType t (Let _ x c1 c2)   = Let t x c1 c2
  setType t (Rec _ es)        = Rec t es
  setType t (Cons _ c1 c2)    = Cons t c1 c2
  setType t (Nil _)           = Nil t
  setType t (Elem _ c f)      = Elem t c f
  setType t (Table _ n c k)   = Table t n c k
  setType t (If _ c1 c2 c3)   = If t c1 c2 c3
  
  
instance HasType Param where
    typeOf (ParExpr t e) = t
    typeOf (ParAbstr t p e) = t
    setType t (ParExpr _ e) = ParExpr t e
    setType t (ParAbstr _ p e) = ParAbstr t p e