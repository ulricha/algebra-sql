{-# LANGUAGE GADTs #-}
module Ferry.TypedCore.Data.TypedCore where
    
import Ferry.Front.Data.Base
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type

data Op where
    Op :: String -> Op

data CoreExpr where
    BinOp :: FType -> Op -> CoreExpr -> CoreExpr -> CoreExpr
    UnaOp :: FType -> Op -> CoreExpr -> CoreExpr
    Constant :: FType -> Const -> CoreExpr
    Var  :: FType -> String -> CoreExpr
    App :: FType -> CoreExpr -> Param -> CoreExpr
    Let :: FType -> String -> CoreExpr -> CoreExpr -> CoreExpr
    Rec :: FType -> [RecElem] -> CoreExpr
    Cons :: FType -> CoreExpr -> CoreExpr -> CoreExpr
    Nil :: FType -> CoreExpr
    Elem :: FType -> CoreExpr -> String -> CoreExpr
    Table :: FType -> String -> [Column] -> [Key] -> CoreExpr
    If :: FType -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr

data RecElem where
    RecElem :: FType -> String -> CoreExpr -> RecElem

data Param where
     ParExpr :: FType -> CoreExpr -> Param
     ParAbstr :: FType -> Pattern -> CoreExpr -> Param

data Pattern where
    PVar :: String -> Pattern
    Pattern :: [String] -> Pattern

data Column where
     Column :: String -> FType -> Column

data Key where
    Key :: [String] -> Key

