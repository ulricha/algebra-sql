-- This module provides functions to replace references within queries.
module Database.Algebra.SQL.Query.Substitution
    ( replaceReferencesSelectStmt
    ) where

import Control.Monad (liftM)

import qualified Database.Algebra.SQL.Query as Q

type SubstitutionFunction = Q.ReferenceType -> Q.FromExpr

-- | Replaces all references in this 'Q.SelectStmt' with the result from the
-- given function.
replaceReferencesSelectStmt :: SubstitutionFunction
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

replaceReferencesSelectColumn :: SubstitutionFunction
                              -> Q.SelectColumn
                              -> Q.SelectColumn
replaceReferencesSelectColumn r (Q.SCAlias e a) =
    Q.SCAlias (replaceReferencesExtendedExpr r e) a

replaceReferencesSelectColumn r (Q.SCExpr e)    =
    Q.SCExpr $ replaceReferencesExtendedExpr r e

replaceReferencesExtendedExpr :: SubstitutionFunction
                              -> Q.ExtendedExpr
                              -> Q.ExtendedExpr
replaceReferencesExtendedExpr r (Q.EEBase v)        =
    Q.EEBase $ replaceReferencesValueExprTemplate replaceReferencesExtendedExpr
                                                  r
                                                  v

replaceReferencesExtendedExpr r (Q.EEWinFun fun part ord frame) =
    Q.EEWinFun (replaceReferencesWindowFunction r fun)
               (liftM (replaceReferencesAggrExpr r) part)
               (map (replaceReferencesWindowOrderExpr r) ord)
               frame
replaceReferencesExtendedExpr r (Q.EEAggrExpr ae) =
    Q.EEAggrExpr $ replaceReferencesAggrExpr r ae

-- | Generic value expression substitution function.
replaceReferencesValueExprTemplate :: (SubstitutionFunction -> a -> a)
                                   -> SubstitutionFunction
                                   -> Q.ValueExprTemplate a
                                   -> Q.ValueExprTemplate a
replaceReferencesValueExprTemplate replaceReferencesRec r ve = case ve of
    Q.VEBinApp bf fe se ->
        Q.VEBinApp bf
                   (replaceReferencesRec r fe)
                   (replaceReferencesRec r se)
    Q.VEUnApp uf e      -> Q.VEUnApp uf (replaceReferencesRec r e)
    Q.VECase cE tE eE   -> Q.VECase (replaceReferencesRec r cE)
                                    (replaceReferencesRec r tE)
                                    (replaceReferencesRec r eE)
    Q.VEExists q        -> Q.VEExists $ replaceReferencesValueQuery r q
    Q.VEIn ae q         -> Q.VEIn (replaceReferencesRec r ae)
                                  (replaceReferencesValueQuery r q)
    Q.VEColumn _ _      -> ve
    Q.VEValue _         -> ve

replaceReferencesColumnExpr :: SubstitutionFunction
                            -> Q.ColumnExpr
                            -> Q.ColumnExpr
replaceReferencesColumnExpr r (Q.CEBase v) =
    Q.CEBase $ replaceReferencesValueExprTemplate replaceReferencesColumnExpr
                                                  r
                                                  v

replaceReferencesAggrExpr :: SubstitutionFunction
                          -> Q.AggrExpr
                          -> Q.AggrExpr
replaceReferencesAggrExpr r (Q.AEBase v)        =
    Q.AEBase $ replaceReferencesValueExprTemplate replaceReferencesAggrExpr
                                                  r
                                                  v
replaceReferencesAggrExpr r (Q.AEAggregate v f) =
    Q.AEAggregate (liftM (replaceReferencesColumnExpr r) v) f

replaceReferencesFromPart :: SubstitutionFunction
                          -> Q.FromPart
                          -> Q.FromPart
replaceReferencesFromPart r (Q.FPAlias e a c) =
    Q.FPAlias (replaceReferencesFromExpr r e) a c

replaceReferencesFromExpr :: SubstitutionFunction
                          -> Q.FromExpr
                          -> Q.FromExpr
replaceReferencesFromExpr r (Q.FESubQuery q) =
    Q.FESubQuery $ replaceReferencesValueQuery r q

replaceReferencesFromExpr r (Q.FEVariable v) = r v

replaceReferencesFromExpr _ t                = t

replaceReferencesValueQuery :: SubstitutionFunction
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

replaceReferencesOrderExpr :: SubstitutionFunction
                           -> Q.OrderExpr
                           -> Q.OrderExpr
replaceReferencesOrderExpr r (Q.OE ee d) =
    Q.OE (replaceReferencesExtendedExpr r ee) d

replaceReferencesWindowOrderExpr :: SubstitutionFunction
                                 -> Q.WindowOrderExpr
                                 -> Q.WindowOrderExpr
replaceReferencesWindowOrderExpr r (Q.WOE ae d) =
    Q.WOE (replaceReferencesAggrExpr r ae) d


replaceReferencesWindowFunction :: SubstitutionFunction
                                -> Q.WindowFunction
                                -> Q.WindowFunction
replaceReferencesWindowFunction r (Q.WFMax a) = 
    Q.WFMax (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFMin a)   = 
    Q.WFMin (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFSum a)   = 
    Q.WFSum (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFAvg a)   = 
    Q.WFAvg (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFAll a)   = 
    Q.WFAll (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFAny a)   = 
    Q.WFAny (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFFirstValue a)   = 
    Q.WFFirstValue (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction r (Q.WFLastValue a)   = 
    Q.WFLastValue (replaceReferencesColumnExpr r a)

replaceReferencesWindowFunction _ Q.WFCount     = 
    Q.WFCount

replaceReferencesWindowFunction _ Q.WFRank      = 
    Q.WFRank

replaceReferencesWindowFunction _ Q.WFDenseRank = 
    Q.WFDenseRank

replaceReferencesWindowFunction _ Q.WFRowNumber = 
    Q.WFRowNumber
