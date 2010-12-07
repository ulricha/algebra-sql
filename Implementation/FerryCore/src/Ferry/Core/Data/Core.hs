{-| Untyped core data type}
{-# LANGUAGE GADTs #-}
module Ferry.Core.Data.Core where

import Ferry.Common.Data.Base

-- | An identifier is represented by a string
type Ident = String

-- | Operator constructor
data Op where
    Op :: String -> Op

-- | Datatype for building untyped core ASTs
data CoreExpr where
    BinOp :: Op -> CoreExpr -> CoreExpr -> CoreExpr
--    UnaOp :: Op -> CoreExpr -> CoreExpr
    Constant :: Const -> CoreExpr
    Var  :: String -> CoreExpr
    App :: CoreExpr -> Param -> CoreExpr
    Let :: String -> CoreExpr -> CoreExpr -> CoreExpr
    Rec :: [RecElem] -> CoreExpr
    Cons :: CoreExpr -> CoreExpr -> CoreExpr
    Nil :: CoreExpr
    Elem :: CoreExpr -> String -> CoreExpr
    Table :: String -> [Column] -> [Key] -> CoreExpr
    If :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr

-- | Record elements
data RecElem where
    RecElem :: String -> CoreExpr -> RecElem
    
-- | Function arguments
-- In future, that is when defunctionalisation is implemented function arguments should just be expressions.
data Param where
     ParExpr :: CoreExpr -> Param
     ParAbstr :: Pattern -> CoreExpr -> Param
 
-- | Patterns   
data Pattern where
    PVar :: String -> Pattern
    Pattern :: [String] -> Pattern
    
-- | Database table column
data Column where
     Column :: String -> Type -> Column

-- | Database column type    
data Type
    = TInt 
    | TFloat 
    | TString 
    | TBool
    | TUnit

-- | Database table key    
data Key where
    Key :: [String] -> Key
