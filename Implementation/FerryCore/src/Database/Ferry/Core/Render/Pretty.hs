-- | Pretty print instance for database types
module Database.Ferry.Core.Render.Pretty where

import Database.Ferry.Core.Data.Core
import Database.Ferry.Common.Render.Pretty

instance Pretty Type where
    pretty a _ = prettyTy a

prettyTy :: Type -> String
prettyTy TUnit = "()"
prettyTy TInt = "Int"
prettyTy TFloat = "Float" 
prettyTy TString = "String" 
prettyTy TBool = "Bool"    