module Ferry.Front.Data.Base where
    
type Identifier = String

data Const = CInt Integer
           | CFloat Double
           | CBool Bool
           | CString String
    deriving (Show, Eq)

class VarContainer a where
    vars :: a -> [Identifier]