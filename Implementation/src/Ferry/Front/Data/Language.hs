{-# LANGUAGE GADTs #-}
module Ferry.Front.Data.Language where
    
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base
    
data Op where
     Op :: Meta -> String -> Op
     deriving (Show, Eq)

data Expr where
     UnOp         :: Meta -> Op -> Expr -> Expr
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
    
data QCompr where
    FerryCompr     :: Meta -> [(Pattern, Expr)] -> [BodyElem] -> ReturnElem -> QCompr
    HaskellCompr :: Meta -> QCompr
     deriving (Show, Eq)

data BodyElem where
    For :: Meta -> [(Pattern, Expr)] -> BodyElem
    ForLet :: Meta -> [(Pattern, Expr)] -> BodyElem
    ForWhere :: Meta -> Expr -> BodyElem
    ForOrder :: Meta -> [ExprOrder] -> BodyElem 
    GroupBy :: Meta -> Maybe Expr -> [Expr] -> Maybe Pattern -> BodyElem
    GroupWith :: Meta -> Maybe Expr -> [Expr] -> Maybe Pattern -> BodyElem
     deriving (Show, Eq)
     
data ReturnElem where
    Return :: Meta -> Expr -> Maybe (Pattern, [BodyElem], ReturnElem) -> ReturnElem
    deriving (Show, Eq)

data ExprOrder where
    ExprOrder :: Meta -> Expr -> Order -> ExprOrder
     deriving (Show, Eq)
     
data Pattern where
    PVar :: Meta -> String -> Pattern
    PPat :: Meta -> [String] -> Pattern
     deriving (Show, Eq)
     
data Order where
     Ascending  :: Meta -> Order
     Descending :: Meta -> Order
     deriving (Show, Eq)
         
data Cardinality where
     One  :: Meta -> Cardinality
     Many :: Meta -> Cardinality
     deriving (Show, Eq)
     
data Key where
    Key :: Meta -> [String] -> Key
     deriving (Show, Eq)
     
data Column where
    Column :: Meta -> String -> Type -> Column
     deriving (Show, Eq)
     
data Binding where
    Binding :: Meta -> String -> Expr -> Binding
     deriving (Show, Eq)
     
data RecElem where
    TrueRec :: Meta -> String -> Expr -> RecElem
    TuplRec :: Meta -> Int -> Expr -> RecElem
     deriving (Show, Eq)
     
data Type where
     TInt    :: Meta -> Type
     TFloat  :: Meta -> Type
     TString :: Meta -> Type
     TBool   :: Meta -> Type 
     deriving (Show, Eq)
     
data Arg where
    AExpr  :: Meta -> Expr -> Arg
    AAbstr :: Meta -> Pattern -> Expr -> Arg
 deriving (Show, Eq)
