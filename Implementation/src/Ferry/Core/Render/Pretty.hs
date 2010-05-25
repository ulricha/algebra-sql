module Ferry.Core.Render.Pretty where

import Ferry.Core.Data.Core

prettyTy :: Type -> String
prettyTy TInt = "Int"
prettyTy TFloat = "Float" 
prettyTy TString = "String" 
prettyTy TBool = "Bool"    