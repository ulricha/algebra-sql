{- | Defines substitutions -}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Ferry.TypedCore.Data.Substitution where
    
import Database.Ferry.TypedCore.Data.Type

import qualified Data.Map as M

-- | A substitution is either a substitution over a type or over a label
type Subst = (TSubst, RSubst)
-- | A substitution is a mapping from a record label to a new record label
type RSubst = M.Map RLabel RLabel
-- | A substitution is a mapping from a type variable to a type
type TSubst = M.Map FType FType

-- | The class substitutable exposes a function that applies a substitution to
-- datatypes that are an instance of it.
class Substitutable a where
    apply :: Subst -> a -> a
