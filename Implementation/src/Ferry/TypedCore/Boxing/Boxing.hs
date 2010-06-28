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
box = undefined