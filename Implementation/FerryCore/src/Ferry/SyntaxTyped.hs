{-| Everything related to typed core-}
module Ferry.SyntaxTyped 
(
Ident,
Identifier, Const (..),
Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..),
int, float, string, bool, list, var, rec, fn, genT, (.->), TyScheme (..), Qual (..), Pred (..), FType (..), RLabel (..), FTFn (..), HasType, typeOf
)
where
import Ferry.Common.Data.Base    
import Ferry.TypedCore.Data.TypedCore (Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..), Ident)
import Ferry.TypedCore.Data.Type (int, float, string, bool, list, var, rec, fn, genT, (.->), TyScheme (..), Qual (..), Pred (..), FType (..), RLabel (..), FTFn (..), HasType, typeOf)
import Ferry.TypedCore.Data.Instances() 
