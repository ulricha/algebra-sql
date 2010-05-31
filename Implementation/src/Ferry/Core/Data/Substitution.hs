{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.Core.Data.Substitution where
    
import Ferry.Core.Data.Base
import Ferry.Core.Data.Type

import qualified Data.Set as S
import qualified Data.Map as M

data Subst where
    Subst :: Ident -> FType -> Subst
    (:*:) :: Subst -> Subst -> Subst
    Id :: Subst
    

    
class Substitutable a where
    apply' :: S.Set Ident -> Subst -> a -> a
    
apply :: Substitutable a => Subst -> a -> a
apply s a = apply' S.empty s a
