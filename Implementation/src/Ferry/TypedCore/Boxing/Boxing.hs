module Ferry.TypedCore.Boxing.Boxing where

import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Instances
import Ferry.TypedCore.Data.Type
import Ferry.Front.Data.Base

import Control.Monad.Reader


runBoxing :: CoreExpr -> CoreExpr
runBoxing = fst . flip runReader emptyEnv . box

data Box = Atom
         | List
         | BFn Box Box
                       
type BoxEnv = [(Identifier, Box)]

emptyEnv :: BoxEnv
emptyEnv = []    

type Boxing = Reader BoxEnv

addToEnv :: Identifier -> Box -> Boxing a -> Boxing a
addToEnv i b e = local ((:)(i, b)) e 
                 
fromEnv :: Identifier -> Boxing Box
fromEnv i = do
              env <- ask
              case lookup i env of
                  Just x -> return x
                  Nothing -> error $ "Identifier: " ++ i ++ " not found in env during boxing."

boxOp :: Box -> Box -> CoreExpr -> CoreExpr
boxOp Atom List = boxFn
boxOp List Atom = unboxFn
boxOp _ _ = id

boxFn :: CoreExpr -> CoreExpr
boxFn e = App t (Var t' "box") $ ParExpr t e
  where 
    t@(q :=> ty) = typeOf e
    t' = q :=> ty .-> ty

unboxFn :: CoreExpr -> CoreExpr
unboxFn e = App t (Var t' "unBox") $ ParExpr t e
  where 
    t@(q :=> ty) = typeOf e
    t' = q :=> ty .-> ty 
    
box :: CoreExpr -> Boxing (CoreExpr, Box)
box c@(Constant _ _) = (c, Atom)
box n@(Nil _)        = (n, List)
box (Cons t e1 e2)   = do
                         (e1', phi) <- box e1
                         (e2', phi2) <- box e2 
                         return (Cons t (boxOp phi star e1') (boxOp phi2 list e2), List)
box (Elem t e s) = do
                      (e', phi) <- box e
                      return $ Elem t (boxOp phi atom e') s
box t@(Table _ _ _ _) = (t, List)
box (If t e1 e2 e3) = do
                        (e1', phi1) <- box e1
                        (e2', phi2) <- box e2
                        (e3', phi3) <- box e3
                        if phi2 == phi3
                            then return (If t (boxOp phi1 atom e1') e2' e3', phi3)
                            else return (If t (boxOp phi1 atom e1') (boxOp phi2 atom e2') (boxOp phi3 atom e3'), atom) 

{-
BinOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr -> CoreExpr
UnaOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr
Var  :: (Qual FType) -> String -> CoreExpr
App :: (Qual FType) -> CoreExpr -> Param -> CoreExpr
Let :: (Qual FType) -> String -> CoreExpr -> CoreExpr -> CoreExpr
Rec :: (Qual FType) -> [RecElem] -> CoreExpr
-}