module Ferry.Core.Data.Instances where
    
import Ferry.Core.Data.Base
import Ferry.Core.Data.Type
import Ferry.Core.Substitution
import Ferry.Core.TypedCore

instance Substitutable FType where
  apply' _ Id t                    = t -- Do not recurse a whole structure for identity substitution
  apply' b s (FList t)             = FList $ apply' b s t 
  apply' b s (FFn t1 t2)           = FFn (apply' b s t1) (apply' b s t2)
  apply' b s (FRec rs)             = FRec $ S.map (\(n, t) -> (n, apply' b s t)) rs
  apply' b (s1 :*: s2) v@(FVar _)  = apply' b s1 $ apply' b s2 v
  apply' b (Subst s t1) v@(FVar i) = case S.member i b || s /= i of
                                        True -> v
                                        False -> t1
  apply' _ _    t                  = t -- If the substitution is not applied to a container type or variable just stop primitives cannot be substituted

instance Substitutable TyScheme where
  apply' b s (Forall i t) = Forall i $ apply' (S.insert i b) s t
  apply' b s (FType t) = FType $ apply' b s t
    
instance Substitutable TyEnv where
  apply' b s m = M.map (apply' b s) m
  
instance Substitutable CoreExpr where
  apply' b Id e                = e
  apply' b s (BinOp t o c1 c2) = BinOp (apply' b s t) (apply' b s o) (apply' b s c1) (apply' b s c2)
  apply' b s (UnOp t o c)      = UnaOp (apply' b s t) (apply' b s o) (apply' b s c)
  
{-
BinOp :: FType -> Op -> CoreExpr -> CoreExpr -> CoreExpr
UnaOp :: FType -> Op -> CoreExpr -> CoreExpr
Constant :: FType -> Const -> CoreExpr
Var  :: FType -> String -> CoreExpr
App :: FType -> CoreExpr -> [Param] -> CoreExpr
Let :: FType -> String -> CoreExpr -> CoreExpr -> CoreExpr
Rec :: FType -> [RecElem] -> CoreExpr
List :: FType ->[CoreExpr] -> CoreExpr
Elem :: FType -> CoreExpr -> String -> CoreExpr
Table :: FType -> String -> [Column] -> [Key] -> CoreExpr
If :: FType -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
-}

{- | Instances of VarContainer class-}
  
instance VarContainer FType where
  ftv (FVar a)    = S.singleton a
  ftv (FList t)   = ftv t
  ftv (FRec s)    = S.unions $ map (ftv . snd) $ S.toList s
  ftv (FFn t1 t2) = ftv t1 `S.union` ftv t2
  ftv _           = S.empty

instance VarContainer TyScheme where
  ftv (Forall i t) = S.delete i $ ftv t 
  ftv (FType t)    = ftv t

instance VarContainer TyEnv where
  ftv m = S.unions $ M.elems $ M.map ftv m