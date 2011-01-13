{- | Datatypes describing typed AST -}
{-# LANGUAGE GADTs #-}
module Ferry.TypedCore.Data.TypedCore where
    
import Ferry.Common.Data.Base(Const)
import Ferry.TypedCore.Data.Type

type Ident = String

data Op where
    Op :: String -> Op
        deriving (Show)

data CoreExpr where
    BinOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr -> CoreExpr
--    UnaOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr
    Constant :: (Qual FType) -> Const -> CoreExpr
    Var  :: (Qual FType) -> String -> CoreExpr
    App :: (Qual FType) -> CoreExpr -> Param -> CoreExpr
    Let :: (Qual FType) -> String -> CoreExpr -> CoreExpr -> CoreExpr
    Rec :: (Qual FType) -> [RecElem] -> CoreExpr
    Cons :: (Qual FType) -> CoreExpr -> CoreExpr -> CoreExpr
    Nil :: (Qual FType) -> CoreExpr
    Elem :: (Qual FType) -> CoreExpr -> String -> CoreExpr
    Table :: (Qual FType) -> String -> [Column] -> [Key] -> CoreExpr
    If :: (Qual FType) -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
    deriving (Show)

data RecElem where
    RecElem :: (Qual FType) -> String -> CoreExpr -> RecElem
    deriving (Show)

data Param where
     ParExpr :: (Qual FType) -> CoreExpr -> Param
     ParAbstr :: (Qual FType) -> [String] -> CoreExpr -> Param
         deriving (Show)

{-
data Pattern where
    PVar :: String -> Pattern
    Pattern :: [String] -> Pattern
        deriving (Show)
-}

data Column where
     Column :: String -> FType -> Column
         deriving (Show)

data Key where
    Key :: [String] -> Key
        deriving (Show)

