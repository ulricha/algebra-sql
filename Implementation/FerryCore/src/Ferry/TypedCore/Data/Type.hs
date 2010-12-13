{-| Type language -}
{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Type where

import qualified Data.Set as S
import qualified Data.Map as M

type Ident = String

-- | Type environment is a mapping from identifiers to type schemes
type TyEnv = M.Map Ident TyScheme

type TyGens = Int
type RecGens = Int

-- | A type scheme represents a quantified type
data TyScheme where
    Forall :: TyGens -> RecGens -> Qual FType -> TyScheme
 deriving Show

infix 5 :=> 

-- | A qualified type is a type with some predicates ([predicates] :=> type)
data Qual t where
  (:=>) :: [Pred] -> t -> Qual t
   deriving Show

-- | Predicates relating to records
data Pred where
 IsIn :: String -> FType -> Pred -- | name `IsIn` t -> t is a record (or type variable) that contains at least a field name
 Has :: FType -> RLabel -> FType -> Pred -- | Similaar to IsIn but now with a type for the name
  deriving (Show, Eq)

-- | Type language 
data FType where
    FGen :: Int -> FType -- | Generalised type variable
    FUnit :: FType -- | ()
    FInt :: FType -- | Int
    FFloat :: FType -- | Float
    FString :: FType -- | String
    FBool :: FType -- | Bool
    FList :: FType -> FType -- | [a]
    FVar :: Ident -> FType -- | a
    FRec :: [(RLabel, FType)] -> FType -- | {x1 :: t1, ..., xn :: tn} 
    FFn :: FType -> FType -> FType -- | t1 -> t2
    FTF :: FTFn -> FType -> FType -- | f t1
 deriving (Show, Eq, Ord)

-- | Is t a primitive type? 
isPrim :: FType -> Bool
isPrim FInt = True
isPrim FFloat = True
isPrim FString = True
isPrim FBool = True
isPrim (FRec ls) = and $ map (isPrim . snd) ls
isPrim _ = False

-- | Language for record labels
data RLabel where
    RLabel :: String -> RLabel
    RGen :: Int -> RLabel -- | Generalised record label
    RVar :: String -> RLabel
 deriving (Show, Eq, Ord)

-- | Type functions 
data FTFn where
    Tr :: FTFn
    Tr' :: FTFn
 deriving (Show, Eq, Ord)

-- * Function used to construct types 
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

-- | A varcontainer can contain type variables, or record variables
class VarContainer a where
   ftv :: a -> S.Set Ident
   frv :: a -> S.Set Ident
   hasQVar :: a -> Bool

-- | Everything that contains a type.   
class HasType a where
  typeOf :: a -> Qual FType
  setType :: Qual FType -> a -> a
  
