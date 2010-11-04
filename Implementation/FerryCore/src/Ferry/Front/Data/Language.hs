{-# LANGUAGE GADTs #-}
module Ferry.Front.Data.Language where
    
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base

-- Operator data type, containing meta information and operators string    
data Op where
     Op :: Meta -> String -> Op
     deriving (Show, Eq)

-- Ferry front expressions
data Expr where
--     UnOp         :: Meta -> Op -> Expr -> Expr
     BinOp        :: Meta -> Op -> Expr -> Expr -> Expr
     Const        :: Meta -> Const -> Expr
     Var          :: Meta -> Identifier -> Expr
     App          :: Meta -> Expr -> [Arg] -> Expr
     If           :: Meta -> Expr -> Expr -> Expr -> Expr
     Record       :: Meta -> [RecElem] -> Expr
     Paren        :: Meta -> Expr -> Expr
     List         :: Meta -> [Expr] -> Expr
     Elem         :: Meta -> Expr -> Either String Integer -> Expr
     Lookup       :: Meta -> Expr -> Expr -> Expr 
     Let          :: Meta -> [Binding] -> Expr -> Expr
     Table        :: Meta -> String -> [Column] -> [Key] -> Expr
     Relationship :: Meta -> Cardinality -> Expr -> Cardinality -> Expr -> Key -> Key -> Expr
     QComp        :: Meta -> QCompr -> Expr
     deriving (Show, Eq)

-- | Query comprehensions    
data QCompr where
    FerryCompr     :: Meta -> [(Pattern, Expr)] -> [BodyElem] -> ReturnElem -> QCompr
    HaskellCompr :: Meta -> Expr -> HaskellBody -> QCompr
     deriving (Show, Eq)
     
-- | Data type for haskell list comprehensions
data HaskellBody where
    HFor :: Meta -> Pattern -> Expr -> HaskellBody
    HLet :: Meta -> Pattern -> Expr -> HaskellBody
    HGuard :: Meta -> Expr -> HaskellBody
    HCart :: Meta -> HaskellBody -> HaskellBody -> HaskellBody
    HZip :: Meta -> HaskellBody -> HaskellBody -> HaskellBody
     deriving (Show, Eq)
    
-- | Data type for ferry comprehensions
data BodyElem where
    For :: Meta -> [(Pattern, Expr)] -> BodyElem
    ForLet :: Meta -> [(Pattern, Expr)] -> BodyElem
    ForWhere :: Meta -> Expr -> BodyElem
    ForOrder :: Meta -> [ExprOrder] -> BodyElem 
    Group :: Meta -> Group -> Maybe Expr -> [Expr] -> Maybe Pattern -> BodyElem
     deriving (Show, Eq)

-- | Types of grouping
data Group where
    GBy :: Group
    GWith :: Group
     deriving (Show, Eq)
     
-- | Return the result of an expression
data ReturnElem where
    Return :: Meta -> Expr -> Maybe (Pattern, [BodyElem], ReturnElem) -> ReturnElem
    deriving (Show, Eq)

-- | Order the result of the expression in Order
data ExprOrder where
    ExprOrder :: Meta -> Expr -> Order -> ExprOrder
     deriving (Show, Eq)

-- | Pattern for function arguments     
data Pattern where
    PVar :: Meta -> String -> Pattern
    PPat :: Meta -> [String] -> Pattern
     deriving (Show, Eq)

-- | Ordering directions     
data Order where
     Ascending  :: Meta -> Order
     Descending :: Meta -> Order
     deriving (Show, Eq)

-- | Cardinality         
data Cardinality where
     One  :: Meta -> Cardinality
     Many :: Meta -> Cardinality
     deriving (Show, Eq)

-- | A database key exists out of multiple columns     
data Key where
    Key :: Meta -> [String] -> Key
     deriving (Show, Eq)

-- | A Column description consisting of name and type     
data Column where
    Column :: Meta -> String -> Type -> Column
     deriving (Show, Eq)

-- | Binding, binds an expr to a variable     
data Binding where
    Binding :: Meta -> String -> Expr -> Binding
     deriving (Show, Eq)

-- | Record element, either a tuple component or a true named record element     
data RecElem where
    TrueRec :: Meta -> Either Expr String -> Maybe Expr -> RecElem
    TuplRec :: Meta -> Int -> Expr -> RecElem
     deriving (Show, Eq)

-- | Database types     
data Type where
     TInt    :: Meta -> Type
     TFloat  :: Meta -> Type
     TString :: Meta -> Type
     TBool   :: Meta -> Type 
     TUnit   :: Meta -> Type
     deriving (Show, Eq)

-- | Arguments in function application     
data Arg where
    AExpr  :: Meta -> Expr -> Arg
    AAbstr :: Meta -> [Pattern] -> Expr -> Arg
 deriving (Show, Eq)
