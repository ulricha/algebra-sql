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

unifyRecords :: [(String, FType)] -> [(String, FType)] -> AlgW ()
unifyRecords ((s, t):r1) r2 = case lookup s r2 of
                                Nothing -> throwError $ UnificationRecError r1 r2
                                Just a -> do
                                            unify t a
                                            unifyRecords r1 $ L.delete (s, a) r2
unifyRecords []         [] = pure ()
unifyRecords r1         r2 = throwError $ UnificationRecError r1 r2 
