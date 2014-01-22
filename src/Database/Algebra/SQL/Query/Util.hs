-- | This module exports useful functions for working with the 'Query' ADT.
module Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkPCol
    , mkSubQuery
    , affectsSortOrder
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

-- Check whether we need an expression within an ORDER BY / GROUP BY / PARTITION
-- BY clause, based on a simple heuristic. The main purpose is to eliminate
-- single values, everything else is optional. This means that some expressions
-- have no effect on the sort order but will still return 'True'.
affectsSortOrder :: Q.ValueExpr -> Bool
affectsSortOrder e = case e of
    -- A constant value won't affect the sort order.
    Q.VEValue _        -> False
    Q.VEColumn _ _     -> True
    Q.VECast e1 _      -> affectsSortOrder e1
    Q.VEBinApp _ e1 e2 -> affectsSortOrder e1 || affectsSortOrder e2
    Q.VENot e1         -> affectsSortOrder e1
    -- We have no correlated queries (in open tiles), but in case we get some,
    -- this is the most flexible solution.
    Q.VEExists _       -> True
    Q.VEIn _ _         -> True

