module Ferry.TypedCore.Rewrite.OpRewrite where
    
import Ferry.TypedCore.Data.Type
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Convert.Traverse
import Ferry.TypedCore.Data.Instances

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
     rules = mFoldCore {binOpF = eqRewrite}

eqRewrite :: Qual FType -> Op -> Rewrite CoreExpr -> Rewrite CoreExpr -> Rewrite CoreExpr
eqRewrite qt (Op "==") e1 e2 = do
                                 e1' <- e1
                                 e2' <- e2
                                 v1 <- getFreshIdentifier
                                 v2 <- getFreshIdentifier
                                 let (_ :=> ty1) = typeOf e1'
                                 case ty1 of
                                     FList t -> eqListExpr v1 v2 e1' e2'
                                     FRec t  -> eqRecExpr v1 v2 e1' e2' 
                                     t       -> return $ BinOp qt (Op "==") e1' e2'
eqRewrite qt o e1 e2 = do
                        e1' <- e1
                        e2' <- e2
                        return $ BinOp qt o e1' e2'

eqRecExpr :: String -> String -> CoreExpr -> CoreExpr -> Rewrite CoreExpr
eqRecExpr id1 id2 val1 val2 = do
                                let t1@(q1 :=> ty1@(FRec ls1)) = typeOf val1
                                let t2@(q2 :=> ty2@(FRec ls2)) = typeOf val2
                                let var1 = Var t1 id1
                                let var2 = Var t2 id2
                                let eqs = [recElemEq l (q1 :=> ty) var1 var2 | (RLabel l, ty) <- ls1]
                                eqs' <- sequence $ map rewrite' eqs
                                return $ Let ([] :=> FBool) id1 val1
                                        $ Let ([] :=> FBool) id2 val2
                                         $ foldl1 andExpr eqs'
                                
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
                                return $ Let ([] :=> FBool) id1 val1 
                                        $ Let ([] :=> FBool) id2 val2 
                                          $ andExpr (eqLength var1 var2) elEq
    
-- | Chain two boolean expression together in an and relation    
andExpr :: CoreExpr -> CoreExpr -> CoreExpr
andExpr = BinOp ([] :=> FBool) (Op "&&")

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

-- | All variable node                   
allN :: CoreExpr
allN = Var ([] :=> list FBool .-> FBool) "all"

mapN :: Qual FType -> Qual FType -> CoreExpr
mapN (q1 :=> t1) (q2 :=> t2) = Var (q1 `L.union` q2 :=> t1 .-> t2 .-> list FBool) "map"
                   
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

-- | Create a typed let binding node                                          
binding :: String -> CoreExpr -> CoreExpr -> CoreExpr
binding s e eb = Let (typeOf eb) s e eb

-- | Zip two list
zipC :: CoreExpr -> CoreExpr -> CoreExpr
zipC e1 e2 = let ty1@(q1 :=> t1) = typeOf e1
                 ty2@(q2 :=> t2) = typeOf e2
                 zipV = zipF ty1 ty2
                 (q :=> zipT) = zippedTy ty1 ty2 
                 app1T = q :=> t2 .-> zipT
              in App (q :=> zipT) (App app1T zipV (ParExpr ty1 e1)) (ParExpr ty2 e2)

-- | Return the type that two lists that are zipped would result in
zippedTy :: Qual FType -> Qual FType -> Qual FType
zippedTy (q1 :=> (FList t1)) (q2 :=> (FList t2)) = (q1 `L.union` q2) :=> (FList $ rec [(RLabel "1", t1), (RLabel "2", t2)])


-- | Create the zip variable node with specialized function type
zipF :: Qual FType -> Qual FType -> CoreExpr
zipF (q1 :=> FList t1) (q2 :=> FList t2) = Var ((q1 `L.union` q2) :=> FList t1 .-> FList t2 .-> (FList $ rec [(RLabel "1", t1), (RLabel "2", t2)])) "zip"


-- | Count variable node, needs a specialized type in the AST and therefore still expects the type of the list
countF :: Qual FType -> CoreExpr
countF (q :=> t) = Var (q :=> t .-> int) "count"  

-- | Wrap an expression that is to be passed as an argument to a function
wrapArg :: CoreExpr -> Param
wrapArg e = ParExpr (typeOf e) e

eq :: CoreExpr -> CoreExpr -> CoreExpr
eq e1 e2 = let (q1 :=> t1) = typeOf e1
               (q2 :=> t2) = typeOf e2
            in BinOp (q1 `L.union` q2 :=> t1 .-> t2 .-> FBool) (Op "==") e1 e2
             