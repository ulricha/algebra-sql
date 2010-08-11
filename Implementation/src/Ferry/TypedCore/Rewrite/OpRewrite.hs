module Ferry.TypedCore.Rewrite.OpRewrite where
    
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Convert.Traverse
import Ferry.TypedCore.Data.Instances
import Ferry.TypedCore.Rewrite.Combinators

import qualified Data.List as L


import Control.Monad.State

type Rewrite = State Int

getFreshIdentifier :: Rewrite String
getFreshIdentifier = do
                        n <- get
                        put $ n + 1
                        return $ "_rw" ++ show n

getFreshVar :: Rewrite (Qual FType -> CoreExpr)
getFreshVar = do
                i <- getFreshIdentifier
                return (\t -> Var t i)
                
runRewrite :: Rewrite a -> a
runRewrite i = fst $ runState i 1
                
rewrite :: CoreExpr -> CoreExpr
rewrite = runRewrite . rewrite' 

rewrite' :: CoreExpr -> Rewrite CoreExpr
rewrite' = traverse rules
    where
     rules = mFoldCore {binOpF = opRewrite, appF = appRewrite}

appRewrite :: Qual FType -> Rewrite CoreExpr -> Rewrite Param -> Rewrite CoreExpr
appRewrite qt e arg = do
                        e' <- e
                        arg' <- arg
                        case (e', arg') of
                            (Var _ "fst", ParExpr _ e2) -> return $ Elem qt e2 "1"
                            (Var _ "snd", ParExpr _ e2) -> return $ Elem qt e2 "2"
                            _                           -> return $ App qt e' arg'

opRewrite :: Qual FType -> Op -> Rewrite CoreExpr -> Rewrite CoreExpr -> Rewrite CoreExpr
opRewrite qt (Op op) e1 e2 = do
                              e1' <- e1
                              e2' <- e2
                              v1 <- getFreshIdentifier
                              v2 <- getFreshIdentifier
                              let (_ :=> ty1) = typeOf e1'
                              case (ty1, op) of
                                  (FList t, "==") -> liftM (addBindings v1 v2 e1' e2') $ eqListExpr v1 v2 e1' e2'
                                  (FRec t, "==") -> liftM (addBindings v1 v2 e1' e2') $ eqRecExpr v1 v2 e1' e2'
                                  (ty, "!=") -> liftM (addBindings v1 v2 e1' e2') $ notEq e1' e2'
                                  (FList t, "<") -> liftM (addBindings v1 v2 e1' e2') $ ordList v1 v2 e1' e2' 
                                  (FRec t, "<") -> liftM (addBindings v1 v2 e1' e2') $ ordRec "<" v1 v2 e1' e2'
                                  (FList t, ">") -> liftM (addBindings v1 v2 e1' e2') $ ordList v2 v1 e2' e1' 
                                  (FRec t, ">") -> liftM (addBindings v1 v2 e1' e2') $ ordRec ">" v1 v2 e1' e2'
                                  (t, "<=") -> liftM (addBindings v1 v2 e1' e2') $ opOrEq "<" v1 v2 e1' e2'
                                  (t, ">=") -> liftM (addBindings v1 v2 e1' e2') $ opOrEq ">" v1 v2 e1' e2'
                                  (t, o) -> return $ BinOp qt (Op o) e1' e2'

addBindings :: String -> String -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
addBindings v1 v2 val1 val2 val3 = Let ([] :=> FBool) v1 val1 
                                    $ Let ([] :=> FBool) v2 val2 val3
                                    

opOrEq :: String -> String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
opOrEq op id1 id2 v1 v2 = let var1 = Var (typeOf v1) id1
                              var2 = Var (typeOf v2) id2
                           in rewrite' $ BinOp ([] :=> FBool) (Op "||") 
                                            (BinOp ([] :=> FBool) (Op op) var1 var2) 
                                            (BinOp ([] :=> FBool) (Op "==") var1 var2)

ordRec :: String -> String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
ordRec op id1 id2 v1 v2 = let (q :=> FRec ls) = typeOf v1
                              var1 = Var (typeOf v1) id1
                              var2 = Var (typeOf v2) id2
                              els = [(Elem (q :=> t) var1 l, Elem (q :=> t) var2 l) | (RLabel l, t) <- ls]
                           in rewrite' $ recCompExpr op els

recCompExpr :: String -> [(CoreExpr, CoreExpr)] -> CoreExpr
recCompExpr op [(v1, v2)] = BinOp ([] :=> FBool) (Op op) v1 v2
recCompExpr op ((v1, v2):vs) = let opE = BinOp ([] :=> FBool) (Op op) v1 v2
                                   eqE = BinOp ([] :=> FBool) (Op "==") v1 v2
                                in BinOp ([] :=> FBool) (Op "||") opE 
                                    $ BinOp ([] :=> FBool) (Op "&&") eqE 
                                      $ recCompExpr op vs

ordList :: String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
ordList id1 id2 val1 val2 = let t1@(q1 :=> ty1@(FList ls1)) = typeOf val1
                                t2@(q2 :=> ty2@(FList ls2)) = typeOf val2
                                var1 = Var t1 id1
                                var2 = Var t2 id2
                                lens = BinOp ([] :=> FBool) (Op "<") (lengthF var1) (lengthF var2)
                                eqMinPf = (minPF var1 var2) `eq` (minPF var2 var1)
                                ltMinPf = BinOp ([] :=> FBool) (Op "<") (minPF var1 var2) $ minPF var2 var1
                             in rewrite' $ flip orExpr ltMinPf $ andExpr lens eqMinPf
                         
eqRecExpr :: String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
eqRecExpr id1 id2 val1 val2 = do
                                let t1@(q1 :=> ty1@(FRec ls1)) = typeOf val1
                                let t2@(q2 :=> ty2@(FRec ls2)) = typeOf val2
                                let var1 = Var t1 id1
                                let var2 = Var t2 id2
                                let eqs = [recElemEq l (q1 :=> ty) var1 var2 | (RLabel l, ty) <- ls1]
                                eqs' <- sequence $ map rewrite' eqs
                                return $ foldl1 andExpr eqs'
                                
recElemEq :: String -> Qual FType -> CoreExpr -> CoreExpr -> CoreExpr
recElemEq lab (q :=> t) v1 v2 = let el1 = Elem (q :=> t) v1 lab
                                    el2 = Elem (q :=> t) v2 lab
                                 in el1 `eq` el2
                                    
    
                                    
-- | Rewrite of list equality
eqListExpr :: String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
eqListExpr id1 id2 val1 val2 = do
                                let t1@(q1 :=> ty1) = typeOf val1
                                let t2@(q2 :=> ty2) = typeOf val2
                                let var1 = Var t1 id1
                                let var2 = Var t2 id2
                                elEq <- elemEq var1 var2
                                return $ andExpr (eqLength var1 var2) elEq
    
-- | Given two list expressions returns an expression that checks that they have equal length
eqLength :: CoreExpr -> CoreExpr -> CoreExpr
eqLength e1 e2 = let (q1 :=> t1) = typeOf e1
                     (q2 :=> t2) = typeOf e2
                     count1 = App ([] :=> FInt) (countF (q1 :=> t1 .-> FInt)) $ wrapArg e1
                     count2 = App ([] :=> FInt) (countF (q2 :=> t2 .-> FInt)) $ wrapArg e2
                  in BinOp ([] :=> FBool) (Op "==") count1 count2

-- | Given two lists of equal length compute elementwise equality
elemEq :: CoreExpr -> CoreExpr -> Rewrite CoreExpr
elemEq e1 e2 = do
                let t1 = typeOf e1
                let t2 = typeOf e2
                let zipE = zipC e1 e2
                eqA  <- eqAbstr t1 t2
                let (qE :=> tE) = typeOf zipE
                let mapNode = mapN (typeOf eqA) (qE :=> tE)
                let app1 = App (qE :=> tE .-> list FBool) mapNode eqA
                let app2 = App ([] :=> list FBool) app1 (ParExpr (qE :=> tE) zipE)
                return $ App ([] :=> FBool) allN (ParExpr (typeOf app2) app2) 

eqAbstr :: Qual FType -> Qual FType -> Rewrite Param
eqAbstr ty1@(q1 :=> FList t1) ty2@(q2 :=> FList t2) = 
          do
            f <- getFreshIdentifier
            let ty@(q :=> t) = zippedTy ty1 ty2
            let fV = Var ty f
            let el1 = Elem (q1 :=> t1) fV "1"
            let el2 = Elem (q2 :=> t2) fV "2"
            eqE <- rewrite' $ BinOp ([] :=> FBool) (Op "==") el1 el2
            return $ ParAbstr (q :=> t .-> FBool) (PVar f) eqE    

notEq :: CoreExpr -> CoreExpr -> Rewrite CoreExpr
notEq e1 e2 = rewrite' $ notF (BinOp ([] :=> FBool) (Op "==") e1 e2)