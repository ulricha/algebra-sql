{-# LANGUAGE GADTs #-}
module Ferry.TypedCore.Data.TypedCore where
    
import Ferry.Front.Data.Base
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type

data Op where
    Op :: String -> Op

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

data RecElem where
    RecElem :: (Qual FType) -> String -> CoreExpr -> RecElem

data Param where
     ParExpr :: (Qual FType) -> CoreExpr -> Param
     ParAbstr :: (Qual FType) -> Pattern -> CoreExpr -> Param

data Pattern where
    PVar :: String -> Pattern
    Pattern :: [String] -> Pattern

data Column where
     Column :: String -> FType -> Column

data Key where
    Key :: [String] -> Key

