{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Type where

import Ferry.TypedCore.Data.Base

import qualified Data.Set as S
import qualified Data.Map as M

type TyEnv = M.Map Ident TyScheme

data TyScheme where
    Forall :: Ident -> TyScheme -> TyScheme
    QualTy :: QualTy -> TyScheme

data FType where
    FInt :: FType
    FFloat :: FType
    FString :: FType
    FBool :: FType
    FList :: FType -> FType
    FVar :: Ident -> FType
    FRec :: S.Set (String, FType) -> FType 
    FFn :: FType -> FType -> FType
 deriving (Show, Eq, Ord)
 
data QualTy where
    Qual :: String -> Ident -> QualTy -> QualTy
    FType :: FType -> QualTy

class VarContainer a where
   ftv :: a -> S.Set Ident
   
class HasType a where
  typeOf :: a -> FType
  
