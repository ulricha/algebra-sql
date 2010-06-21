{-# LANGUAGE FlexibleInstances #-}
module Ferry.TypedCore.Render.Pretty where

import Char    
import Ferry.Common.Render.Pretty
import Ferry.TypedCore.Data.Type

import qualified Data.Set as S

instance Pretty FType where
  pretty FInt      _ = "Int"
  pretty FFloat    _ = "Float"
  pretty FString   _ = "String"
  pretty FBool     _ = "Bool"
  pretty (FList a) _ = "[" ++ pretty a 0 ++ "]"
  pretty (FVar a)  _ = "a" ++ a
  pretty (FRec a)  _ = case fst $ head a of
                        RLabel r -> case and $ map isDigit $ r of
                                    True -> "(" ++ mapIntersperseConcat (flip pretty 1) ", " (map snd a) ++ ")"
                                    False -> "{" ++ mapIntersperseConcat (flip pretty 1) ", " a ++ "}"
                        otherwise -> "{" ++ mapIntersperseConcat (flip pretty 1) ", " a ++ "}"
  pretty (FFn t1 t2) _ = "(" ++ pretty t1 0 ++ ") -> " ++ pretty t2 0
  pretty (FGen i) _ = "a" ++ show i
  pretty (FRGen i) _ = "qf" ++ show i
  pretty (FRVar x) _ = "f" ++ show x
  
instance (Pretty a) => Pretty (Qual a) where
    pretty (ps :=> t) _ = (mapIntersperseConcat (flip pretty 1) ", " ps) ++ " => " ++ pretty t 1   
    
instance Pretty Pred where
    pretty (IsIn s t) _ = s ++ " " ++ pretty t 1
    pretty (Has r f t) _ = pretty r 1 ++ " <: {" ++ pretty f 1 ++ " ::" ++ pretty t 1 ++ "}"

instance Pretty (RLabel, FType) where
   pretty (n, t) i = pretty n i ++ " :: " ++ pretty t i

instance Pretty RLabel where
    pretty (RLabel n) i = n
    pretty (RGen n) i = "f" ++ show n
    pretty (RVar n) i = "f" ++ show n 
    