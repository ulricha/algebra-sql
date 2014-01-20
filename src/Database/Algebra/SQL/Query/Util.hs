-- | This module exports useful functions for working with the 'Query' ADT.
module Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkPCol
    , mkSubQuery
    , isConstValueExpr
    ) where

import Database.Algebra.SQL.Query as Q

-- | Helper value to construct select statements.
emptySelectStmt :: Q.SelectStmt
emptySelectStmt = Q.SelectStmt [] False [] Nothing [] []

-- | Shorthand to make a prefixed column value expression.
mkPCol :: String
       -> String
       -> Q.ValueExpr
mkPCol p c = Q.VEColumn c $ Just p

-- | Embeds a query into a from part as sub query.
mkSubQuery :: Q.SelectStmt
           -> String
           -> Maybe [String]
           -> Q.FromPart
mkSubQuery sel = Q.FPAlias (Q.FESubQuery $ Q.VQSelect sel)

-- Check whether a value expression is constant as a column expression.
isConstValueExpr :: Q.ValueExpr -> Bool
isConstValueExpr e = case e of
    Q.VEValue _        -> True
    Q.VEColumn _ _     -> False
    Q.VECast e1 _      -> isConstValueExpr e1
    Q.VEBinApp _ e1 e2 -> isConstValueExpr e1 && isConstValueExpr e2
    Q.VENot e1         -> isConstValueExpr e1
    Q.VEExists _       -> True
    Q.VEIn e1 _        -> isConstValueExpr e1

