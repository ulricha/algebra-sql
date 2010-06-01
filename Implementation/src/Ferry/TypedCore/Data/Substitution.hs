{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Substitution where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type

import qualified Data.Set as S
import qualified Data.Map as M

type Subst = M.Map Ident FType

class Substitutable a where
    apply' :: S.Set Ident -> Subst -> a -> a
    
apply :: Substitutable a => Subst -> a -> a
apply s a = if M.null s
              then a
              else apply' S.empty s a
