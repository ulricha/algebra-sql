-- | This module exports useful functions for working with the 'Query' ADT.
module Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkPCol
    , mkSubQuery
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

