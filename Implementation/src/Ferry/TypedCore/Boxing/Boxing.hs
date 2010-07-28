module Ferry.TypedCore.Boxing.Boxing where

import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Instances
import Ferry.TypedCore.Data.Type
import Ferry.Front.Data.Base

import Control.Monad.Reader
                               
import qualified Data.Map as M (lookup)
import Data.Maybe (listToMaybe, fromJust)

runBoxing :: TyEnv -> CoreExpr -> CoreExpr
runBoxing env = fst . flip runReader (env, Nothing, emptyEnv) . box

data Box = Atom
         | List
         | BFn Box Box
       deriving (Eq, Show)
                       
type BoxEnv = [(Identifier, Box)]

type Context = Maybe Box

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

withContext :: Box -> Boxing a -> Boxing a
withContext t = local (\(gE, _, bE) -> (gE, Just t, bE)) 

getFromContext :: Boxing (Maybe Box) 
getFromContext = do
                    (_, c, _) <- ask
                    return c
                                                     
noContext :: Boxing a -> Boxing a
noContext = local (\(gE, _, bE) -> (gE, Nothing, bE))
 
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
boxOp Atom List = unboxFn
boxOp List Atom = boxFn
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
                         (e1', psi) <- noContext $ box e1
                         (e2', psi2) <- noContext $ box e2 
                         resultCheck (Cons t (boxOp psi Atom e1') (boxOp psi2 List e2'), List)
box (Elem t e s) = do
                      (e', psi) <- noContext $ box e
                      resultCheck (Elem t (boxOp psi Atom e') s, Atom)
box t@(Table _ _ _ _) = resultCheck (t, List)
box (If t e1 e2 e3) = do
                        (e1', psi1) <- noContext $ box e1
                        (e2', psi2) <- box e2
                        (e3', psi3) <- box e3
                        if psi2 == psi3
                            then resultCheck (If t (boxOp psi1 Atom e1') e2' e3', psi3)
                            else resultCheck (If t (boxOp psi1 Atom e1') (boxOp psi2 Atom e2') (boxOp psi3 Atom e3'), Atom) 
box (Let t s e1 e2) = do
                        (e1', psi1) <- noContext $ box e1
                        (e2', psi2) <- addToEnv s psi1 $ box e2
                        resultCheck (Let t s e1' e2', psi2)
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
                     (e1', psi) <- noContext $ box e1
                     let (psia, psir ) = case psi of
                                           (BFn psia psir) -> (psia, psir)
                                           _               -> error $ show psi ++ "not a function box"   
                     (e2', psi2) <- withContext psia $ boxParam e2
                     resultCheck (App t e1' e2', psir)
box (BinOp t (Op o) e1 e2) = do
                               ty <- fromGam o
                               case ty of
                                   Nothing -> error $ "Non primitive operator during boxing phase, this should not happen: " ++ show o
                                   (Just t') -> do
                                                let (BFn psi1 (BFn psi2 psi3)) = trans $ inst t'
                                                (e1', psi1') <- noContext $ box e1
                                                (e2', psi2') <- noContext $ box e2
                                                resultCheck (BinOp t (Op o) (boxOp psi1' psi1 e1') (boxOp psi2' psi2 e2'), psi3)
{- box (UnaOp t (Op o) e1) = do
                            ty <- fromGam o
                            case ty of
                                Nothing -> error "Non primitive unary operator during boxing phase, this should not happen"
                                (Just t') -> do
                                              let (BFn psi1 psi2) = trans $ inst t'
                                              (e1', psi1') <- noContext $ box e1
                                              resultCheck (UnaOp t (Op o) (boxOp psi1' psi1 e1'), psi2)
-} 

                    
boxRec :: RecElem -> Boxing RecElem 
boxRec (RecElem t x e) = do
                          (e', psi) <- box e
                          return $ RecElem t x (boxOp psi Atom e')    

boxParam :: Param -> Boxing (Param, Box)   
boxParam (ParExpr t e) = do
                           (e', psi) <- noContext $ box e
                           psie <- getFromContext
                           return $ (ParExpr t $ boxOp psi (fromJust psie) e', fromJust psie)
boxParam (ParAbstr t p e) = do
                             let args = getVars p
                             psie <- getFromContext
                             let (asso, boxR) = varsWithBox args $ fromJust psie
                             (e', psi) <- foldr (\(v, t) r -> addToEnv v t r) (withContext boxR $ box e) asso
                             return (ParAbstr t p e', psi)
                             
    
getVars :: Pattern -> [String]
getVars (PVar v) = [v]
getVars (Pattern p) = p  

varsWithBox :: [String] -> Box -> ([(String, Box)], Box)
varsWithBox []             b = ([], b)
varsWithBox (x:xs) (BFn b1 b2) = (\(l, b) -> ((x, b1):l, b)) (varsWithBox xs b2)
varsWithBox _      b           = error $ "varswithBox err, should not happen"   