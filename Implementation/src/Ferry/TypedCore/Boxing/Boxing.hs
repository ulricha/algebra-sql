module Ferry.TypedCore.Boxing.Boxing where

import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Instances
import Ferry.TypedCore.Data.Type
import Ferry.Front.Data.Base

import Control.Monad.Reader
                               
import qualified Data.Map as M (lookup)
import Data.Maybe (listToMaybe)

runBoxing :: TyEnv -> CoreExpr -> CoreExpr
runBoxing env = fst . flip runReader (env, [], emptyEnv) . box

data Box = Atom
         | List
         | BFn Box Box
                       
type BoxEnv = [(Identifier, Box)]

type Context = [Box]

emptyEnv :: BoxEnv
emptyEnv = []    

type Boxing = Reader (TyEnv, Context, BoxEnv)

addToEnv :: Identifier -> Box -> Boxing a -> Boxing a
addToEnv i b = local (\(gE, cE, bE) -> (gE, cE, (i, b):bE))

fromGam :: Identifier -> Boxing (Maybe TyScheme)
fromGam i = do
             (g, _, _) <- ask
             return $ M.lookup i g
                 
fromEnv :: Identifier -> Boxing Box
fromEnv i = do
              (_, _, env) <- ask
              case lookup i env of
                  Just x -> return x
                  Nothing -> error $ "Identifier: " ++ i ++ " not found in env during boxing."

addToContext :: Box -> Boxing a -> Boxing a
addToContext t = local (\(gE, cE, bE) -> (gE, t:cE, bE)) 

getFromContext :: Boxing (Maybe Box) 
getFromContext = do
                    (_, c, _) <- ask
                    return $ listToMaybe c
                             
popFromContext :: Boxing a -> Boxing a
popFromContext e = do
                    res <- getFromContext
                    case res of
                        Nothing -> e
                        Just _ -> local (\(gE, cE, bE) -> (gE, tail cE, bE)) e
                        

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
box = undefined
{-box c@(Constant _ _) = (c, Atom)
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
box (Let t s e1 e2) = do
                        (e1', phi1) <- box e1
                        (e2', phi2) <- addToEnv s phi1 $ box e2'
                        return (Let t s e1' e2', phi2)
box (Var t x) = do 
                  phi <- fromEnv x
                  return (Var t z, phi)
box (Rec t els) = do 
                    els' <- mapM boxRec els
                    return (Rec t els', atom)
box (App t e1 e2) = do
                     undefined
-}                    
boxRec :: RecElem -> Boxing RecElem 
boxRec = undefined
{-
boxRec (RecElem t x e) = do
                          (e', phi) <- box e
                          return $ RecElem t x (boxOp phi atom e')
-}    
{-
BinOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr -> CoreExpr
UnaOp :: (Qual FType) -> Op -> CoreExpr -> CoreExpr
App :: (Qual FType) -> CoreExpr -> Param -> CoreExpr
-}