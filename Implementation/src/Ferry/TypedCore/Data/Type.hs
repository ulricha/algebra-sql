{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Type where

import Ferry.TypedCore.Data.Base

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

type TyEnv = M.Map Ident TyScheme

type TyGens = Int
type RecGens = Int

data TyScheme where
    Forall :: TyGens -> RecGens -> Qual FType -> TyScheme
 deriving Show

infix 5 :=> 

data Qual t where
  (:=>) :: [Pred] -> t -> Qual t
   deriving Show

data Pred where
 IsIn :: String -> FType -> Pred
 Has :: FType -> RLabel -> FType -> Pred
  deriving (Show, Eq)

data FType where
    FGen :: Int -> FType
    FUnit :: FType
    FInt :: FType
    FFloat :: FType
    FString :: FType
    FBool :: FType
    FList :: FType -> FType
    FVar :: Ident -> FType
    FRec :: [(RLabel, FType)] -> FType
    FFn :: FType -> FType -> FType
    FTF :: FTFn -> FType -> FType
 deriving (Show, Eq, Ord)

data RLabel where
    RLabel :: String -> RLabel
    RGen :: Int -> RLabel
    RVar :: String -> RLabel
 deriving (Show, Eq, Ord)
 
data FTFn where
    Tr :: FTFn
    Tr' :: FTFn
 deriving (Show, Eq, Ord)
 
int :: FType
int = FInt
float :: FType
float = FFloat
string :: FType
string = FString
bool ::  FType
bool = FBool
list :: FType -> FType
list t = FList t
var :: Ident -> FType
var i = FVar i
rec :: [(RLabel, FType)] -> FType
rec s = FRec s
fn :: FType -> FType -> FType
fn t1 t2 = FFn t1 t2
genT :: Int -> FType
genT i = FGen i  

infixr 6 .->

(.->) :: FType -> FType -> FType 
t1 .-> t2 = fn t1 t2

class VarContainer a where
   ftv :: a -> S.Set Ident
   frv :: a -> S.Set Ident
   hasQVar :: a -> Bool
   
class HasType a where
  typeOf :: a -> Qual FType
  setType :: Qual FType -> a -> a
  
