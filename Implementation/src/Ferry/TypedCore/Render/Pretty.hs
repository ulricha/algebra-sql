module Ferry.TypedCore.Render.Pretty where
    
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
  pretty (FRec a)  _ = "{" ++ mapIntersperseConcat (flip pretty 1) ", " (map snd $ S.toList a) ++ "}" 
  pretty (FFn t1 t2) _ = "(" ++ pretty t1 0 ++ ") -> " ++ pretty t2 0
  
instance (Pretty a) => Pretty (Qual a) where
    pretty (ps :=> t) _ = (mapIntersperseConcat (flip pretty 1) ", " ps) ++ " => " ++ pretty t 1   
    
instance Pretty Pred where
    pretty (IsIn s t) _ = s ++ " " ++ pretty t 1
    pretty (Has r f t) _ = pretty r 1 ++ " <: {" ++ f ++ " ::" ++ pretty t 1 ++ "}"  
