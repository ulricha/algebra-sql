{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Type where

import Ferry.TypedCore.Data.Base

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

type TyEnv = M.Map Ident TyScheme

data TyScheme where
    Forall :: Ident -> TyScheme -> TyScheme
    QualTy :: Qual FType -> TyScheme

infixr 6 .->

(.->) :: Qual FType -> Qual FType -> Qual FType 
t1 .-> t2 = fn t1 t2

data FType where
    FInt :: FType
    FFloat :: FType
    FString :: FType
    FBool :: FType
    FList :: FType -> FType
    FVar :: Ident -> FType
    FRTy :: Ident -> FType -> FType
    FRec :: S.Set (String, FType) -> FType
    FRecV :: Ident -> FType 
    FFn :: FType -> FType -> FType
 deriving (Show, Eq, Ord)


int :: Qual FType
int = ([]) :=> FInt
float :: Qual FType
float = ([]) :=> FFloat
string :: Qual FType
string = ([]) :=> FString
bool :: Qual FType
bool = ([]) :=> FBool
list :: Qual FType -> Qual FType
list (q :=> t) = q :=> FList t
var :: Ident -> Qual FType
var i = ([]) :=> FVar i
rec :: S.Set (String, FType) -> Qual FType
rec s = ([]) :=> FRec s
rec' :: [Pred] -> Ident -> Qual FType
rec' f r = (f) :=> FRecV r
fn :: Qual FType -> Qual FType -> Qual FType
fn (q1 :=> t1) (q2 :=> t2) = mergeQuals q1 q2 :=> FFn t1 t2  

infix 5 :=> 

data Qual t where
    (:=>) :: [Pred] -> t -> Qual t
    
data Pred where
    IsIn :: String -> FType -> Pred
    Has :: FType -> String -> FType -> Pred
     deriving Eq


class VarContainer a where
   ftv :: a -> S.Set Ident
   
class HasType a where
  typeOf :: a -> Qual FType
  
mergeQuals :: [Pred] -> [Pred] -> [Pred]
mergeQuals []     t  = t
mergeQuals t      [] = t
mergeQuals (p:ps) t  = if L.elem p t then mergeQuals ps t else mergeQuals ps (p:t)

mergeQuals' :: [[Pred]] -> [Pred]
mergeQuals' = foldr mergeQuals []   
