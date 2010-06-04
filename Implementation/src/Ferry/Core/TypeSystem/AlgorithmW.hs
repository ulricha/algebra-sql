module Ferry.Core.TypeSystem.AlgorithmW (runAlgW, algW) where

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

import Control.Applicative hiding (Const(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error


algW :: C.CoreExpr -> AlgW CoreExpr
algW (C.Constant c)  = Constant <$> typeOfConst c <*> pure c
algW (C.Var x)       = Var <$> (inst $ lookupVariable x) <*> pure x
algW (C.Let x c1 c2) = Let <$> liftM typeOf c2' <*> pure x <*> c1' <*> c2' 
 where
     c1' = algW c1
     c2' = do
            (q :=> t) <- liftM typeOf c1'
            addToEnv x (QualTy $ q :=> t) (algW c2)
algW (C.Nil) = Nil <$> liftM (\v -> [] :=> FVar v) freshTyVar
algW (C.Cons c1 c2) = do
                        c1' <- algW c1
                        c2' <- algW c2
                        let (q1 :=> t1) = typeOf c1'
                        let (q2 :=> t2) = typeOf c2'
                        unify t2 $ FList t1
                        return $ Cons ((mergeQuals q1 q2) :=> t2) c1' c2'
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
                         applyS $ return $ If (mergeQuals' [q1, q2, q3] :=> t2) c1' c2' c3'                        
                        
{-    
BinOp :: Op -> CoreExpr -> CoreExpr -> CoreExpr
UnaOp :: Op -> CoreExpr -> CoreExpr
App :: CoreExpr -> Param -> CoreExpr
Rec :: [RecElem] -> CoreExpr
Cons :: CoreExpr -> CoreExpr -> CoreExpr
Elem :: CoreExpr -> String -> CoreExpr
Table :: String -> [Column] -> [Key] -> CoreExpr
-}

typeOfConst :: Const -> AlgW (Qual FType)
typeOfConst (CInt _) = pure $ [] :=> FInt
typeOfConst (CFloat _) = pure $ [] :=> FFloat
typeOfConst (CBool _) = pure $ [] :=>  FBool
typeOfConst (CString _) = pure $ [] :=> FString


inst :: AlgW TyScheme -> AlgW (Qual FType)
inst s = do
            s' <- s
            case s' of
                QualTy t -> pure t
                Forall i t -> do
                                freshVar <- freshTyVar
                                localAddSubstitution i (FVar freshVar) (applyS (inst $ pure t))

{-
     Forall :: Ident -> TyScheme -> TyScheme
     FType :: FType -> TyScheme  
           FInt :: FType
           FFloat :: FType
           FString :: FType
           FBool :: FType
           FList :: FType -> FType
           FVar :: Ident -> FType
           FRec :: S.Set (String, FType) -> FType 
           FFn :: FType -> FType -> FType
-}