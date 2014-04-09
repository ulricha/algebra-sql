-- This module provides functions to replace references within queries.
module Database.Algebra.SQL.Query.Substitution
    ( replaceReferencesSelectStmt
    ) where

import Control.Monad (liftM)

import qualified Database.Algebra.SQL.Query as Q

-- | Replaces all references in this 'Q.SelectStmt' with the result from the
-- given function.
replaceReferencesSelectStmt :: (Q.ReferenceType -> Q.FromExpr)
                            -> Q.SelectStmt
                            -> Q.SelectStmt
replaceReferencesSelectStmt r body =
    body
    { Q.selectClause = map (replaceReferencesSelectColumn r)
                           $ Q.selectClause body
    , Q.fromClause = map (replaceReferencesFromPart r) $ Q.fromClause body
    , Q.whereClause = liftM (replaceReferencesColumnExpr r) $ Q.whereClause body
    , Q.groupByClause = map (replaceReferencesColumnExpr r)
                            $ Q.groupByClause body
    , Q.orderByClause = map (replaceReferencesOrderExpr r)
                            $ Q.orderByClause body
    }

replaceReferencesSelectColumn :: (Q.ReferenceType -> Q.FromExpr)
                              -> Q.SelectColumn
                              -> Q.SelectColumn
replaceReferencesSelectColumn r (Q.SCAlias e a)     =
    Q.SCAlias (replaceReferencesAdvancedExpr r e) a

replaceReferencesAdvancedExpr :: (Q.ReferenceType -> Q.FromExpr)
                              -> Q.AdvancedExpr
                              -> Q.AdvancedExpr
replaceReferencesAdvancedExpr r (Q.AEBase v)        =
    Q.AEBase $ replaceReferencesAdvancedExprBase r v
replaceReferencesAdvancedExpr r (Q.AERowNum p o)    =
    Q.AERowNum (liftM (replaceReferencesAdvancedExpr r) p)
               (map (replaceReferencesOrderExpr r) o)

replaceReferencesAdvancedExpr r (Q.AEDenseRank o)   =
    Q.AEDenseRank (map (replaceReferencesOrderExpr r) o)
replaceReferencesAdvancedExpr r (Q.AERank o)        =
    Q.AERank (map (replaceReferencesOrderExpr r) o)
replaceReferencesAdvancedExpr r (Q.AEAggregate v f) =
    Q.AEAggregate (liftM (replaceReferencesColumnExpr r) v) f

-- | Replace references in a 'Q.AdvancedExprBase'.
replaceReferencesAdvancedExprBase :: (Q.ReferenceType -> Q.FromExpr)
                                  -> Q.AdvancedExprBase
                                  -> Q.AdvancedExprBase
replaceReferencesAdvancedExprBase r e = case e of
    Q.VEExists q -> Q.VEExists $ replaceReferencesValueQuery r q
    Q.VEIn ae q  -> Q.VEIn (replaceReferencesAdvancedExpr r ae)
                           (replaceReferencesValueQuery r q)
    _            -> e

replaceReferencesColumnExpr :: (Q.ReferenceType -> Q.FromExpr)
                            -> Q.ColumnExpr
                            -> Q.ColumnExpr
replaceReferencesColumnExpr r (Q.CEBase v) =
    Q.CEBase $ replaceReferencesColumnExprBase r v

-- | Replace references in a 'Q.ColumnExprBase'.
replaceReferencesColumnExprBase :: (Q.ReferenceType -> Q.FromExpr)
                                -> Q.ColumnExprBase
                                -> Q.ColumnExprBase
replaceReferencesColumnExprBase r e = case e of
    Q.VEExists q -> Q.VEExists $ replaceReferencesValueQuery r q
    Q.VEIn ve q  -> Q.VEIn (replaceReferencesColumnExpr r ve)
                           (replaceReferencesValueQuery r q)
    _            -> e

replaceReferencesFromPart :: (Q.ReferenceType -> Q.FromExpr)
                          -> Q.FromPart
                          -> Q.FromPart
replaceReferencesFromPart r (Q.FPAlias e a c) =
    Q.FPAlias (replaceReferencesFromExpr r e) a c

replaceReferencesFromExpr :: (Q.ReferenceType -> Q.FromExpr)
                          -> Q.FromExpr
                          -> Q.FromExpr
replaceReferencesFromExpr r (Q.FESubQuery q) =
    Q.FESubQuery $ replaceReferencesValueQuery r q

replaceReferencesFromExpr r (Q.FEVariable v) = r v

replaceReferencesFromExpr _ t                = t

replaceReferencesValueQuery :: (Q.ReferenceType -> Q.FromExpr)
                            -> Q.ValueQuery
                            -> Q.ValueQuery
replaceReferencesValueQuery r (Q.VQSelect s) =
    Q.VQSelect $ replaceReferencesSelectStmt r s

replaceReferencesValueQuery r
    (Q.VQWith bindings body) =
    Q.VQWith (map f bindings) $ replaceReferencesValueQuery r body
  where f (n, oC, q) = (n, oC, replaceReferencesValueQuery r q)

replaceReferencesValueQuery r
    (Q.VQBinarySetOperation lq rq o) =
    Q.VQBinarySetOperation (replaceReferencesValueQuery r lq)
                           (replaceReferencesValueQuery r rq)
                           o

replaceReferencesValueQuery _ q = q

replaceReferencesOrderExpr :: (Q.ReferenceType -> Q.FromExpr)
                           -> Q.OrderExpr
                           -> Q.OrderExpr
replaceReferencesOrderExpr r (Q.OE e d) =
    Q.OE (replaceReferencesAdvancedExpr r e) d

