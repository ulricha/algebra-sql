module Ferry.Core.TypeSystem.Unification where
    
import Ferry.Core.TypeSystem.Types
import Ferry.TypedCore.Data.Type
import Ferry.Compiler.Error.Error

import Control.Applicative hiding (Const(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Error

import qualified Data.Set as S

unify :: FType -> FType -> AlgW ()
unify FInt        FInt        = pure ()
unify FFloat      FFloat      = pure ()
unify FBool       FBool       = pure ()
unify FString     FString     = pure ()
unify (FList a)   (FList b)   = unify a b
unify (FFn a1 b1) (FFn a2 b2) = unify a1 a2 >> unify b1 b2
unify (FRec r1)   (FRec r2)   = error $ "records are not supported yet"
unify t           v@(FVar a)  = if v == t || S.notMember a (ftv t)
                                    then updateSubstitution a t
                                    else pure ()
unify v@(FVar a)  t           = if v == t || S.notMember a (ftv t)
                                    then updateSubstitution a t
                                    else pure ()
unify a1          a2          = throwError $ UnificationError a1 a2
