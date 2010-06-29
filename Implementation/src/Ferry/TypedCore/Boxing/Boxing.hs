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
       deriving Eq
                       
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
                        
noContext :: Boxing a -> Boxing a
noContext = local (\(gE, _, bE) -> (gE, [], bE))
 
trans :: FType -> Box
trans (FList _) = List
trans (FFn t1 t2) = BFn (trans t1) (trans t2)
trans _           = Atom
    
{-
Heavily simplified inst, it doesn't work as a proper inst
 from the type system it only works for types that are passed to trans.
-}
inst :: TyScheme -> FType
inst (Forall _ _ (_ :=> t)) = t

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


resultCheck :: (CoreExpr, Box) -> Boxing (CoreExpr, Box)
resultCheck (e, psi) = do
                        psir <- getFromContext 
                        case (psir, psi) of
                            (Just p, psi) | p == psi -> return (e, psi) 
                                          | otherwise -> error "Expected box sort doesn't match inferred sort"
                            (Nothing, psi) -> return (e, psi)

    
box :: CoreExpr -> Boxing (CoreExpr, Box)
box c@(Constant _ _) = resultCheck (c, Atom)
box n@(Nil _)        = resultCheck (n, List)
box (Cons t e1 e2)   = do
                         (e1', phi) <- noContext $ box e1
                         (e2', phi2) <- noContext $ box e2 
                         resultCheck (Cons t (boxOp phi Atom e1') (boxOp phi2 List e2), List)
box (Elem t e s) = do
                      (e', phi) <- noContext $ box e
                      resultCheck (Elem t (boxOp phi Atom e') s, Atom)
box t@(Table _ _ _ _) = resultCheck (t, List)
box (If t e1 e2 e3) = do
                        (e1', phi1) <- noContext $ box e1
                        (e2', phi2) <- box e2
                        (e3', phi3) <- box e3
                        if phi2 == phi3
                            then resultCheck (If t (boxOp phi1 Atom e1') e2' e3', phi3)
                            else resultCheck (If t (boxOp phi1 Atom e1') (boxOp phi2 Atom e2') (boxOp phi3 Atom e3'), Atom) 
box (Let t s e1 e2) = do
                        (e1', phi1) <- noContext $ box e1
                        (e2', phi2) <- addToEnv s phi1 $ box e2
                        resultCheck (Let t s e1' e2', phi2)
box (Var t x) = do 
                  ty <- fromGam x
                  case ty of
                      Nothing -> do
                                  psi <- fromEnv x
                                  resultCheck (Var t x, psi)
                      (Just t') -> do
                                   let psi = trans $ inst t' 
                                   resultCheck (Var t x, psi)
box (Rec t els) = do 
                    els' <- mapM (noContext . boxRec) els
                    return (Rec t els', Atom)
box (App t e1 e2) = do
                     undefined
                    
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