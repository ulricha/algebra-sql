{-# LANGUAGE TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Substitution where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type

import qualified Data.Set as S
import qualified Data.Map as M

type Subst = (TSubst, RSubst)
type RSubst = M.Map RLabel RLabel
type TSubst = M.Map FType FType

class Substitutable a where
    apply :: Subst -> a -> a
