-- | This module exports useful functions for working with the 'Query' ADT.
module Database.Algebra.SQL.Query.Util
    ( emptySelectStmt
    , mkPCol
    , mkSubQuery
    , affectsSortOrderCE
    , affectsSortOrderAE
    , affectsSortOrderEE
    ) where

import Database.Algebra.SQL.Query as Q

-- | Helper value to construct select statements.
emptySelectStmt :: Q.SelectStmt
emptySelectStmt = Q.SelectStmt [] False [] [] [] []

-- | Shorthand to make a prefixed column value expression.
mkPCol :: String
       -> String
       -> Q.ValueExprTemplate a
mkPCol p c = Q.VEColumn c $ Just p

-- | Embeds a query into a from part as sub query.
mkSubQuery :: Q.SelectStmt
           -> String
           -> Maybe [String]
           -> Q.FromPart
mkSubQuery sel = Q.FPAlias (Q.FESubQuery $ Q.VQSelect sel)

-- | Check whether we need an expression within an ORDER BY / GROUP BY /
-- PARTITION BY clause, based on a simple heuristic. The main purpose is to
-- eliminate single values, everything else is optional. This means that some
-- expressions have no effect on the sort order but will still return 'True'.
affectsSortOrderValueExprTemplate :: (a -> Bool)
                                  -> Q.ValueExprTemplate a
                                  -> Bool
affectsSortOrderValueExprTemplate affectsSortOrderRec ve = case ve of
    -- A constant value won't affect the sort order.
    Q.VEValue _        -> False
    Q.VEColumn _ _     -> True
    Q.VECast e1 _      -> affectsSortOrderRec e1
    Q.VEBinApp _ e1 e2 -> affectsSortOrderRec e1 || affectsSortOrderRec e2
    Q.VEUnApp _ e1     -> affectsSortOrderRec e1
    Q.VENot e1         -> affectsSortOrderRec e1
    -- We have no correlated queries (in open tiles), but in case we get some,
    -- this is the most flexible solution.
    Q.VEExists _       -> True
    Q.VEIn _ _         -> True
    Q.VECase c t e     ->
        affectsSortOrderRec c || affectsSortOrderRec t || affectsSortOrderRec e

affectsSortOrderCE :: Q.ColumnExpr -> Bool
affectsSortOrderCE (Q.CEBase e) =
    affectsSortOrderValueExprTemplate affectsSortOrderCE e

affectsSortOrderEE :: Q.ExtendedExpr -> Bool
affectsSortOrderEE e = case e of
    EEBase ve         -> affectsSortOrderValueExprTemplate affectsSortOrderEE ve
    EERowNum _ _      -> True
    EEDenseRank _     -> True
    EERank _          -> True
    EEAggrExpr ae     -> affectsSortOrderAE ae

affectsSortOrderAE :: Q.AggrExpr -> Bool
affectsSortOrderAE ae = case ae of
    AEBase ve -> affectsSortOrderValueExprTemplate affectsSortOrderAE ve
    -- TODO
    AEAggregate _ _   -> True

--isMergeable :: Q.SelectStmt -> Bool
--isMergeable (Q.SelectStmt sClause d _ _ [] _) =
--    not $ d || any (usesExtendedExprs . sExpr) sClause
--  where
--    usesExtendedExprs e = case e of
--        EEBase _ -> False
--        _        -> True
--isMergeable _                                 = False

-- | Search for references and try to merge a select stmt at that position.
--deepMergeSelectStmt :: (Int -> (Bool, Q.SelectStmt)) -> Q.SelectStmt -> Q.SelectStmt
--deepMergeSelectStmt lookupFun select =
    
