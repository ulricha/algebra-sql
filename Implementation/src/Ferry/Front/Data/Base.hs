module Ferry.Front.Data.Base where
    
type Identifier = String

data Const = CInt Integer
           | CFloat Double
           | CBool Bool
           | CString String
    deriving (Show, Eq)

class VarContainer a where
    vars :: a -> [Identifier]
    
toString :: Const -> String
toString (CInt i) = show i
toString (CFloat d) = show d
toString (CBool b) = show b
toString (CString s) = s