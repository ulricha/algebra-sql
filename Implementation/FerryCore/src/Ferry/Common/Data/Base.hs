{- | 
This module contains some datatypes and functions that are used at various stages in the compiler.
-}
module Ferry.Common.Data.Base where

-- | Identifiers are represented as strings    
type Identifier = String

-- | Constant values
data Const = CInt Integer
           | CFloat Double
           | CBool Bool
           | CString String
           | CUnit
    deriving (Show, Eq)

-- | Type class for extracting all variables that occur in a value of type a
class VarContainer a where
    vars :: a -> [Identifier]

-- | Print constants    
toString :: Const -> String
toString (CInt i) = show i
toString (CFloat d) = show d
toString (CBool b) = show b
toString (CString s) = s
toString (CUnit) = "()"