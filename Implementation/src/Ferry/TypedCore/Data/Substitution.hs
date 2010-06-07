{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Substitution where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type

import qualified Data.Set as S
import qualified Data.Map as M

type Subst = M.Map FType FType

class Substitutable a where
    apply :: Subst -> a -> a
