{- | Helper functions to construct a typed AST -}
{-# LANGUAGE TemplateHaskell #-}
module Database.Ferry.TypedCore.Rewrite.Combinators where

import Database.Ferry.TypedCore.Data.Type
import Database.Ferry.TypedCore.Data.TypedCore
import Database.Ferry.TypedCore.Data.Instances()
import Database.Ferry.Impossible

import qualified Data.List as L
    
-- | Count variable node, needs a specialized type in the AST and therefore still expects the type of the list
countF :: Qual FType -> CoreExpr
countF (q :=> t) = Var (q :=> t .-> int) "count"  

-- | Wrap an expression that is to be passed as an argument to a function
wrapArg :: CoreExpr -> Param
wrapArg e = ParExpr (typeOf e) e

-- | Apply equality operator to two expressions
eq :: CoreExpr -> CoreExpr -> CoreExpr
eq e1 e2 = BinOp ([] :=> FBool) (Op "==") e1 e2

-- | Apply negation to expression
notF :: CoreExpr -> CoreExpr
notF e = App ([] :=> FBool) (Var ([] :=> FBool .-> FBool) "not") (ParExpr (typeOf e) e)    

-- | Apply length function to expression
lengthF :: CoreExpr -> CoreExpr
lengthF e = let (q :=> t) = typeOf e
            in App ([] :=> FInt) (Var (q :=> t .-> FInt) "length") (ParExpr (typeOf e) e)

minPF :: CoreExpr -> CoreExpr -> CoreExpr
minPF e1 e2 = let (q1 :=> t1) = typeOf e1
                  (q2 :=> t2) = typeOf e2
                  fn' = Var (q1 `L.union` q2 :=> t1 .-> t2 .-> FInt) "minP"
                  app1 = App (q2 :=> t2 .-> FInt) fn' (ParExpr (typeOf e1) e1)
               in App ([] :=> FInt) app1 (ParExpr (typeOf e2) e2)
               
-- | Create the zip variable node with specialized function type
zipF :: Qual FType -> Qual FType -> CoreExpr
zipF (q1 :=> FList t1) (q2 :=> FList t2) = Var ((q1 `L.union` q2) :=> FList t1 .-> FList t2 .-> (FList $ rec [(RLabel "1", t1), (RLabel "2", t2)])) "zip"
zipF _ _ = $impossible

-- | Create a typed let binding node                                          
binding :: String -> CoreExpr -> CoreExpr -> CoreExpr
binding s e eb = Let (typeOf eb) s e eb

-- | Zip two list
zipC :: CoreExpr -> CoreExpr -> CoreExpr
zipC e1 e2 = let ty1 = typeOf e1
                 ty2@(_ :=> t2) = typeOf e2
                 zipV = zipF ty1 ty2
                 (q :=> zipT) = zippedTy ty1 ty2 
                 app1T = q :=> t2 .-> zipT
              in App (q :=> zipT) (App app1T zipV (ParExpr ty1 e1)) (ParExpr ty2 e2)
              
-- | All variable node                   
allN :: CoreExpr
allN = Var ([] :=> list FBool .-> FBool) "all"

mapN :: Qual FType -> Qual FType -> CoreExpr
mapN (q1 :=> t1) (q2 :=> t2) = Var (q1 `L.union` q2 :=> t1 .-> t2 .-> list FBool) "map"

-- | Chain two boolean expression together in an and relation    
andExpr :: CoreExpr -> CoreExpr -> CoreExpr
andExpr = BinOp ([] :=> FBool) (Op "&&")

orExpr :: CoreExpr -> CoreExpr -> CoreExpr
orExpr = BinOp ([] :=> FBool) (Op "||")

-- | Return the type that two lists that are zipped would result in
zippedTy :: Qual FType -> Qual FType -> Qual FType
zippedTy (q1 :=> (FList t1)) (q2 :=> (FList t2)) = (q1 `L.union` q2) :=> (FList $ rec [(RLabel "1", t1), (RLabel "2", t2)])
zippedTy _ _ = $impossible
