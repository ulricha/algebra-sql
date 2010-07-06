module Ferry.TypedCore.Rewrite.OpRewrite where
    
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Convert.Traverse

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
rewrite = runRewrite rewrite' 

rewrite' :: CoreExpr -> Rewrite CoreExpr
rewrite' = traverse rules
    where
     rules = mFoldCore {binOpF = eqRewrite}

eqRewrite :: Qual FType -> Op -> Rewrite CoreExpr -> Rewrite CoreExpr -> Rewrite CoreExpr
eqRewrite qt (Op "==") e1 e2 = do
                                 e1' <- e1
                                 e2' <- e2
                                 v1 <- getFreshIdentifier
                                 v2 <- getFreshIdentifier
                                 let t1@(q1 :=> ty1) = typeOf e1'
                                 let t2@(q2 :=> ty2) = typeOf e2'
                                 let var1 = Var t1 v1
                                 let var2 = Var t2 v2
                                 case ty1 of
                                     FList t -> undefined
                                     FRec t  -> undefined 
                                     t       -> return $ BinOp qt (Op "==") e1' e2'

elemEq :: CoreExpr -> CoreExpr -> CoreExpr
elemEq e1 e2 = let t1 = typeOf e1
                   t2 = typeOf e2
                   zipE = zipC e1 e2
                   eqA  = eqAbstr t1 t2
                   (qE :=> tE) = typeOf zipE
                   mapNode = mapNode (typeOf eqA) (qE :=> tE)
                   app1 = App (qE :=> tE .-> list FBool) mapNode eqA
                   app2 = App (list FBool) app1 (ParExpr (qE :=> tE) zipE)
                in App allN (ParExpr (typeOf app2) app2) 
                   
allN :: CoreExpr
allN = Var ([] :=> list FBool .-> FBool) "all"

mapN :: Qual FType -> Qual FType -> CoreExpr
mapN (q1 :=> t1) (q2 :=> t2) = Var (q1 `L.union` q2 :=> t1 .-> t2 .-> list FBool) "map"
                   
eqAbstr :: Qual FType -> Qual FType -> Rewrite Param
eqAbstr ty1@(q1 :=> t1) ty2@(q2 :=> t2) = 
          do
            f <- getFreshIdentifier
            let ty@(q :=> t) = zippedTy ty1 ty2
            let fV = Var ty f
            let el1 = Elem ty1 fV "1"
            let el2 = Elem ty2 fV "2"
            eqE = rewrite' $ BinOp FBool (Op "==") el1 el2
            return $ ParAbstr (q :=> t .-> FBool) (PVar f) eqE    
                                          
binding :: String -> CoreExpr -> CoreExpr -> CoreExpr
binding s e eb = Let (typeOf eb) s e eb

zipC :: CoreExpr -> CoreExpr -> CoreExpr
zipC e1 e2 = let ty1@(q1 :=> t1) = typeOf e1
                 ty2@(q2 :=> t2) = typeOf e2
                 zipV = zipF ty1 ty2
                 (q :=> zipT) = zippedTy ty1 ty2 
                 app1T = q :=> t2 .-> zipT
              in App (q :=> zipT) (App app1T zipV (ParExpr ty1 e1)) (ParExpr ty2 e2)

zippedTy :: Qual FType -> Qual FType -> Qual FType
zippedTy (q1 :=> t1) (q2 :=> t2) = (q1 `L.union` q2) :=> FList $ rec [(RLabel "1", t1), (RLabel "2", t2)]

zipF :: Qual FType -> Qual FType -> CoreExpr
zipF (q1 :=> (FList t1) (q2 :=> FList t2) = Var ((q1 `L.union` q2) :=> FList t1 .-> FList t2 .-> FList $ rec [(RLabel "1", t1), (RLabel "2", t2)]) "zip"

countF :: Qual FType -> CoreExpr
countF (q :=> t) = Var (q :=> t .-> int) "count"  

