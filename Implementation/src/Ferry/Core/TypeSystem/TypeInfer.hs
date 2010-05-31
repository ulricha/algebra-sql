module Ferry.Core.TypeSystem.TypeInfer where
    
import qualified Ferry.Core.Data.Core as C
import Ferry.Core.Data.TypedCore
import Ferry.Core.Data.Type
import Ferry.Core.Data.Substitution 
import Ferry.Core.Data.Base

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

type AlgW = ReaderT TyEnv (State (Int, Subst))

runAlgW :: AlgW a -> (a, Subst)
runAlgW a = (x, s)
   where
    (x, (_, s)) = runState (runReaderT a M.empty) (1, Id)

freshTyVar :: AlgW Ident 
freshTyVar = do
                (n, theta) <- get
                put (n + 1, theta)
                return (show n)
                
lookupVariable :: Ident -> AlgW TyScheme
lookupVariable i = do 
                liftM (M.findWithDefault err i) ask
            where 
                err = error $ "Variable " ++ i ++ " not bound in env." 
                
bindVariable :: Ident -> TyScheme -> AlgW a -> AlgW a
bindVariable x t = local (M.insert x t)
    
algW :: C.CoreExpr -> AlgW CoreExpr
algW = undefined