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
    , Q.whereClause = liftM (replaceReferencesValueExpr r) $ Q.whereClause body
    , Q.groupByClause = map (replaceReferencesValueExpr r)
                            $ Q.groupByClause body
    , Q.orderByClause = map (replaceReferencesOrderExpr r)
                            $ Q.orderByClause body
    }

replaceReferencesSelectColumn :: (Q.ReferenceType -> Q.FromExpr)
                              -> Q.SelectColumn
                              -> Q.SelectColumn
replaceReferencesSelectColumn r (Q.SCAlias e a) =
    Q.SCAlias (replaceReferencesSelectExpr r e) a

replaceReferencesSelectExpr :: (Q.ReferenceType -> Q.FromExpr)
                            -> Q.SelectExpr
                            -> Q.SelectExpr
replaceReferencesSelectExpr r (Q.SEValueExpr v)   =
    Q.SEValueExpr $ replaceReferencesValueExpr r v
replaceReferencesSelectExpr r (Q.SERowNum p o)    =
    Q.SERowNum (liftM (replaceReferencesValueExpr r) p)
               (map (replaceReferencesOrderExpr r) o)

replaceReferencesSelectExpr r (Q.SEDenseRank o)   =
    Q.SEDenseRank (map (replaceReferencesOrderExpr r) o)
replaceReferencesSelectExpr r (Q.SERank o)        =
    Q.SERank (map (replaceReferencesOrderExpr r) o)
replaceReferencesSelectExpr r (Q.SEAggregate v f) =
    Q.SEAggregate (liftM (replaceReferencesValueExpr r) v) f

replaceReferencesValueExpr :: (Q.ReferenceType -> Q.FromExpr)
                           -> Q.ValueExpr
                           -> Q.ValueExpr
replaceReferencesValueExpr r (Q.VEExists q)          =
    Q.VEExists $ replaceReferencesValueQuery r q

replaceReferencesValueExpr r (Q.VEIn e q)            =
    Q.VEIn (replaceReferencesValueExpr r e)
           (replaceReferencesValueQuery r q)

replaceReferencesValueExpr r (Q.VECast targetExpr t) =
    Q.VECast (replaceReferencesValueExpr r targetExpr) t

replaceReferencesValueExpr r (Q.VEBinApp bf fe se)   =
    Q.VEBinApp bf
               (replaceReferencesValueExpr r fe)
               (replaceReferencesValueExpr r se)

replaceReferencesValueExpr r (Q.VEUnApp uf e)        =
    Q.VEUnApp uf (replaceReferencesValueExpr r e)

replaceReferencesValueExpr r (Q.VENot e)             =
    Q.VENot (replaceReferencesValueExpr r e)

replaceReferencesValueExpr r (Q.VECase cE tE eE)     =
    Q.VECase (replaceReferencesValueExpr r cE)
             (replaceReferencesValueExpr r tE)
             (replaceReferencesValueExpr r eE)

replaceReferencesValueExpr _ e                       = e

replaceReferencesFromPart :: (Q.ReferenceType -> Q.FromExpr)
                          -> Q.FromPart
                          -> Q.FromPart
replaceReferencesFromPart r (Q.FPAlias e a c) =
    Q.FPAlias (replaceReferencesFromExpr r e) a c

replaceReferencesFromPart r (Q.FPInnerJoin lp rp jc) =
    Q.FPInnerJoin (replaceReferencesFromPart r lp)
                  (replaceReferencesFromPart r rp)
                  (replaceReferencesValueExpr r jc)


replaceReferencesFromExpr :: (Q.ReferenceType -> Q.FromExpr)
                          -> Q.FromExpr
                          -> Q.FromExpr
replaceReferencesFromExpr r (Q.FESubQuery q) =
    Q.FESubQuery $ replaceReferencesValueQuery r q

replaceReferencesFromExpr r (Q.FEVariable v) = r v

replaceReferencesFromExpr _ t = t

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
    Q.OE (replaceReferencesValueExpr r e) d

