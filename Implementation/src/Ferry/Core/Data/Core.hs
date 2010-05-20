module Ferry.Core.Data.Core where

import Ferry.Front.Data.Base

data Op where
    Op :: String -> Op

data CoreExpr where
    BinOp :: Op -> CoreExpr -> CoreExpr -> CoreExpr
    UnaOp :: Op -> CoreExpr -> CoreExpr
    Constant :: Const -> CoreExpr
    Var  :: String -> CoreExpr
    App :: CoreExpr -> Param -> CoreExpr
    Let :: String -> CoreExpr -> CoreExpr -> CoreExpr
    Rec :: [RecElem] -> CoreExpr
    List :: [CoreExpr] -> CoreExpr
    Elem :: CoreExpr -> String -> CoreExpr
    Table :: String -> [Column] -> [Key] -> CoreExpr
    If :: CoreExpr -> CoreExpr -> CoreExpr

data RecElem where
    RecElem :: String -> CoreExpr -> CoreExpr
    
data Param where
     ParExpr :: CoreExpr -> Param
     ParAbstr :: Pattern -> CoreExpr -> Param
    
data Pattern where
    Var :: String -> Pattern
    Pattern :: [String] -> Pattern
    
data Column where
     Column :: String -> Type -> Column
    
data Type
    = TInt 
    | TFloat 
    | TString 
    | TBool
    
data Key where
    Key :: [String] -> Key
