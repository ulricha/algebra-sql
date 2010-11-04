module Ferry.Core.Render.Pretty where

import Ferry.Core.Data.Core
import Ferry.Common.Render.Pretty

instance Pretty Type where
    pretty a _ = prettyTy a

prettyTy :: Type -> String
prettyTy TUnit = "()"
prettyTy TInt = "Int"
prettyTy TFloat = "Float" 
prettyTy TString = "String" 
prettyTy TBool = "Bool"    