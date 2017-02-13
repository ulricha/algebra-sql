{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}

-- | Type checking of table algebra plans.
module Database.Algebra.Table.Typing
    ( inferTATypes
    , tyBinOp
    , tyUnOp
    , tyNullOp
    ) where

import           Data.List
import           Text.Printf
import           Control.Monad.Except
import qualified Data.Set.Monad                                    as S
import qualified Data.IntMap as IM

import           Database.Algebra.Table.Lang
import           Database.Algebra.Dag
import           Database.Algebra.Dag.Common
import           Database.Algebra.Rewrite.Properties

-- | Look up the type of an attribute in a row type
colTy :: MonadError String m => Attr -> S.Set TypedAttr -> m ATy
colTy k s =
    case S.toList [ b | (a, b) <- s, k == a ] of
        [b] -> pure b
        []  -> throwError $ printf "Cols.colTy: %s not in %s" k (show s)
        _   -> throwError $ printf "Cols.colTy: duplicate %s in %s" k (show s)

----------------------------------------------------------------------------
-- Infer types of scalar expressions.

-- | Throw a type error for atomic functions
funTyErr :: (MonadError String m, Show f) => f -> [ATy] -> m a
funTyErr f tys = throwError $ printf "type error for %s: %s" (show f) (show tys)

relFunTy :: MonadError String m => BinFun -> ATy -> ATy -> m ATy
relFunTy f ty1 ty2
    | ty1 == ty2 = pure ABool
    | otherwise  = funTyErr f [ty1, ty2]

numTy :: ATy -> Bool
numTy ty = ty `elem` [AInt,ADec,ADouble,ADate]

numFunTy :: MonadError String m => BinFun -> ATy -> ATy -> m ATy
numFunTy f ty1 ty2
    | ty1 == ty2 && numTy ty1 && numTy ty2 = pure ty1
    | otherwise                            = funTyErr f [ty1, ty2]

stringPredFunTy :: MonadError String m => BinFun -> ATy -> ATy -> m ATy
stringPredFunTy _ AStr AStr = pure ABool
stringPredFunTy f ty1  ty2  = funTyErr f [ty1, ty2]

boolFunTy :: MonadError String m => BinFun -> ATy -> ATy -> m ATy
boolFunTy _ ABool ABool = pure ABool
boolFunTy f ty1  ty2    = funTyErr f [ty1, ty2]

binAppTy :: MonadError String m => BinFun -> ATy -> ATy -> m ATy
binAppTy f ty1 ty2 = do
    case f of
        Gt        -> relFunTy f ty1 ty2
        Lt        -> relFunTy f ty1 ty2
        LtE       -> relFunTy f ty1 ty2
        GtE       -> relFunTy f ty1 ty2
        Eq        -> relFunTy f ty1 ty2
        NEq       -> relFunTy f ty1 ty2
        Contains  -> stringPredFunTy f ty1 ty2
        SimilarTo -> stringPredFunTy f ty1 ty2
        Like      -> stringPredFunTy f ty1 ty2
        And       -> boolFunTy f ty1 ty2
        Or        -> boolFunTy f ty1 ty2
        Plus      -> numFunTy f ty1 ty2
        Minus     -> numFunTy f ty1 ty2
        Times     -> numFunTy f ty1 ty2
        Div       -> numFunTy f ty1 ty2
        Modulo    ->
            if ty1 == AInt && ty2 == AInt
            then pure AInt
            else funTyErr f [ty1, ty2]
        Concat    ->
            if ty1 == AStr && ty2 == AStr
            then pure AStr
            else funTyErr f [ty1, ty2]
        Coalesce  ->
            if ty1 == ty2
            then pure ty1
            else funTyErr f [ty1, ty2]

trigFunTy :: MonadError String m => UnFun -> ATy -> m ATy
trigFunTy _ ADouble = pure ADouble
trigFunTy f ty      = funTyErr f [ty]

dateExtractFunTy :: MonadError String m => UnFun -> ATy -> m ATy
dateExtractFunTy _ ADate = pure AInt
dateExtractFunTy f ty    = funTyErr f [ty]

unAppTy :: MonadError String m => UnFun -> ATy -> m ATy
unAppTy f ty =
    case f of
        Not         ->
            if ty == ABool
            then pure ABool
            else funTyErr f [ty]
        (Cast t)    -> pure t
        Sin         -> trigFunTy f ty
        Cos         -> trigFunTy f ty
        Tan         -> trigFunTy f ty
        ASin        -> trigFunTy f ty
        ACos        -> trigFunTy f ty
        ATan        -> trigFunTy f ty
        Log         -> trigFunTy f ty
        Ln          -> trigFunTy f ty
        Sqrt        -> trigFunTy f ty
        Exp         -> trigFunTy f ty
        SubString{} ->
            if ty == AStr
            then pure AStr
            else funTyErr f [ty]
        DateDay     -> dateExtractFunTy f ty
        DateMonth   -> dateExtractFunTy f ty
        DateYear    -> dateExtractFunTy f ty
        IsNull      -> pure ABool

ternaryAppTy :: MonadError String m => TernaryFun -> ATy -> ATy -> ATy -> m ATy
ternaryAppTy f ty1 ty2 ty3 = case f of
    Between ->
        if ty1 == ty2 && ty1 == ty3
        then pure ty1
        else funTyErr f [ty1, ty2, ty3]
    If      ->
        if ty1 == ABool && ty2 == ty3
        then pure ty2
        else funTyErr f [ty1, ty2, ty3]

valType :: AVal -> ATy
valType (VInt _)    = AInt
valType (VStr _)    = AStr
valType (VBool _)   = ABool
valType (VDouble _) = ADouble
valType (VDec _)    = ADec
valType (VDate _)   = ADate

exprTy :: MonadError String m => S.Set TypedAttr -> Expr -> m ATy
exprTy childCols expr =
    case expr of
        ColE c                 -> colTy c childCols
        ConstE v               -> pure $ valType v
        BinAppE f e1 e2        -> do
            ty1 <- exprTy childCols e1
            ty2 <- exprTy childCols e2
            binAppTy f ty1 ty2
        UnAppE f e             -> (exprTy childCols e) >>= unAppTy f
        TernaryAppE f e1 e2 e3 -> do
            ty1 <- exprTy childCols e1
            ty2 <- exprTy childCols e2
            ty3 <- exprTy childCols e3
            ternaryAppTy f ty1 ty2 ty3

----------------------------------------------------------------------------
-- Type inference for aggregate functions

numAggr :: MonadError String m => ATy -> m ATy
numAggr AInt    = pure AInt
numAggr ADec    = pure ADec
numAggr ADouble = pure ADouble
numAggr ty      = throwError $ printf "numAggr: %s" (show ty)

aggrTy :: MonadError String m => S.Set TypedAttr -> AggrType -> m ATy
aggrTy childCols aggr =
    case aggr of
        All e           -> do
            eTy <- exprTy childCols e
            if eTy == ABool
                then pure ABool
                else opTyErr aggr [childCols]
        Any e           -> do
            eTy <- exprTy childCols e
            if eTy == ABool
                then pure ABool
                else opTyErr aggr [childCols]
        CountStar       -> pure AInt
        Count e         -> void (exprTy childCols e) >> pure AInt
        CountDistinct e -> void (exprTy childCols e) >> pure AInt
        Avg e           -> exprTy childCols e >>= numAggr
        Max e           -> exprTy childCols e >>= numAggr
        Min e           -> exprTy childCols e >>= numAggr
        Sum e           -> exprTy childCols e >>= numAggr

winFunTy :: MonadError String m => S.Set TypedAttr -> WinFun -> m ATy
winFunTy childCols aggr =
    case aggr of
        WinAll e        -> do
            eTy <- exprTy childCols e
            if eTy == ABool
                then pure ABool
                else opTyErr aggr [childCols]
        WinAny e        -> do
            eTy <- exprTy childCols e
            if eTy == ABool
                then pure ABool
                else opTyErr aggr [childCols]
        WinCount        -> pure AInt
        WinAvg e        -> exprTy childCols e >>= numAggr
        WinMax e        -> exprTy childCols e >>= numAggr
        WinMin e        -> exprTy childCols e >>= numAggr
        WinSum e        -> exprTy childCols e >>= numAggr
        WinFirstValue e -> exprTy childCols e
        WinLastValue e  -> exprTy childCols e

----------------------------------------------------------------------------
-- Schema inference for tablealgebra operators

tyErrShowOp :: MonadError String m => Show o => o -> String -> m a
tyErrShowOp o msg = throwError $ printf "Inference failed for %s\n%s" (show o) msg

opTyErr :: (MonadError String m, Show o) => o -> [S.Set TypedAttr] -> m a
opTyErr o rows = throwError $ printf "%s\n%s" (show o) (concat $ intersperse "\n" (map show rows))

tyNullOp :: MonadError String m => NullOp -> m (S.Set TypedAttr)
tyNullOp op =
    case op of
        LitTable (_, schema)   -> pure $ S.fromList schema
        TableRef (_, attrs, _) -> pure $ S.fromList attrs


tyUnOp :: MonadError String m => S.Set TypedAttr -> UnOp -> m (S.Set TypedAttr)
tyUnOp childCols op = flip catchError (tyErrShowOp op) $
    case op of
        WinFun ((resCol, fun), pes, ses, _) -> do
            mapM_ (exprTy childCols) pes
            checkSortSpec childCols ses
            wTy <- winFunTy childCols fun
            pure $ S.insert (resCol, wTy) childCols
        RowNum (resCol, ses, pes) -> do
            mapM_ (exprTy childCols) pes
            checkSortSpec childCols ses
            pure $ S.insert (resCol, AInt) childCols
        RowRank (resCol, ses)   -> do
            checkSortSpec childCols ses
            pure $ S.insert (resCol, AInt) childCols
        Rank (resCol, ses)      -> do
            checkSortSpec childCols ses
            pure $ S.insert (resCol, AInt) childCols
        Project projs         ->
            S.fromList <$> (forM projs $ \(a, e) -> do
                ty <- exprTy childCols e
                pure (a, ty))
        Select p              -> do
            pTy <- exprTy childCols p
            if pTy == ABool
                then pure childCols
                else opTyErr op [childCols]
        Distinct _            -> pure childCols
        Aggr (afuns, ges)  -> do
            aTys <- forM afuns $ \(afun, a) -> do
                aTy <- aggrTy childCols afun
                pure (a, aTy)
            gTys <- forM ges $ \(a, e) -> do
                gTy <- exprTy childCols e
                pure (a, gTy)
            pure $ S.union (S.fromList aTys) (S.fromList gTys)
        Serialize (ref, key, ord, items) ->
            let cols = (map (\(PayloadCol c e) -> (c,e)) items)
                       ++ (map (\(RefCol c e) -> (c,e)) ref)
                       ++ (map (\(OrdCol (c, _) e) -> (c,e)) ord)
                       ++ (map (\(KeyCol c e) -> (c,e)) key)
            in S.fromList <$> mapM (\(c,e) -> (c,) <$> exprTy childCols e) cols


tyBinOp :: MonadError String m => S.Set TypedAttr -> S.Set TypedAttr -> BinOp -> m (S.Set TypedAttr)
tyBinOp leftCols rightCols op = flip catchError (tyErrShowOp op) $
    case op of
        Cross _         -> rowTyDisjunct leftCols rightCols >> pure (S.union leftCols rightCols)
        ThetaJoin p     -> do
            rowTyDisjunct leftCols rightCols
            checkPred leftCols rightCols p
            pure $ S.union leftCols rightCols
        LeftOuterJoin p -> do
            rowTyDisjunct leftCols rightCols
            checkPred leftCols rightCols p
            pure $ S.union leftCols rightCols
        SemiJoin p      -> do
            rowTyDisjunct leftCols rightCols
            checkPred leftCols rightCols p
            pure leftCols
        AntiJoin p      -> do
            rowTyDisjunct leftCols rightCols
            checkPred leftCols rightCols p
            pure leftCols
        DisjUnion _     -> unionCompat leftCols rightCols >> pure leftCols
        Difference _    -> unionCompat leftCols rightCols >> pure leftCols

checkSortSpec :: MonadError String m => S.Set TypedAttr -> [SortSpec] -> m ()
checkSortSpec cols sortSpec = mapM_ (exprTy cols . fst) sortSpec

checkPred :: MonadError String m => S.Set TypedAttr -> S.Set TypedAttr -> [(Expr, Expr, JoinRel)] -> m ()
checkPred leftCols rightCols p = mapM_ (checkConj leftCols rightCols) p

checkConj :: MonadError String m => S.Set TypedAttr -> S.Set TypedAttr -> (Expr, Expr, JoinRel) -> m ()
checkConj leftCols rightCols p@(leftExpr, rightExpr, _)= do
    leftTy  <- exprTy leftCols leftExpr
    rightTy <- exprTy rightCols rightExpr
    if leftTy == rightTy
       then pure ()
       else throwError $ printf "Cols.checkConj: %s" (show p)

unionCompat :: MonadError String m => S.Set TypedAttr -> S.Set TypedAttr -> m ()
unionCompat leftCols rightCols
    | leftCols == rightCols = pure ()
    | otherwise             = throwError $ printf "Cols.unionCompat: %s /= %s" (show leftCols) (show rightCols)

rowTyDisjunct :: MonadError String m => S.Set TypedAttr -> S.Set TypedAttr -> m ()
rowTyDisjunct s1 s2 =
    if S.null $ (fmap fst s1) `S.intersection` (fmap fst s2)
    then pure ()
    else throwError $ printf "row type not disjunct:\n%s\n%s" (show s1) (show s2)

--------------------------------------------------------------------------------

-- | Infer a type for all operators in an MA Dag.
inferTATypes :: MonadError String m => AlgebraDag TableAlgebra -> m (NodeMap (S.Set TypedAttr))
inferTATypes = inferBottomUpE tyOp

childTy :: MonadError String m => AlgNode -> NodeMap (S.Set TypedAttr) -> m (S.Set TypedAttr)
childTy n m =
    case IM.lookup n m of
        Just ty -> pure ty
        Nothing -> throwError $ printf "No type for node %d" n

enrichTyErr :: MonadError String m => m a -> AlgNode -> [S.Set TypedAttr] -> m a
enrichTyErr ma n csTys = catchError ma $ \msg ->
    throwError $ printf "TA type inference failed at node %d\n%s\nmessage: %s" n csTyMsg msg
  where
    csTyMsg = concat $ intersperse "\n" $ map show csTys

tyOp :: MonadError String m
     => NodeMap TableAlgebra
     -> TableAlgebra
     -> AlgNode
     -> NodeMap (S.Set TypedAttr)
     -> m (S.Set TypedAttr)
tyOp _ op n tyMap =
    case op of
        BinOp o c1 c2  -> do
            t1 <- childTy c1 tyMap
            t2 <- childTy c2 tyMap
            enrichTyErr (tyBinOp t1 t2 o) n [t1, t2]
        (UnOp o c) -> do
            t <- childTy c tyMap
            enrichTyErr (tyUnOp t o) n [t]
        (NullaryOp o) -> enrichTyErr (tyNullOp o) n []
        TerOp{} -> error "no ternary TA operators"
