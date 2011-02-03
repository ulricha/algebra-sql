{-| This module performs boxing on a typed AST.
This is an essential step in the compilation pipeline.
It is described in more detail in:
http://www-db.informatik.uni-tuebingen.de/files/publications/avalanche-safe-linq.pdf
Figure 7 rule 17 and 18
-}
module Database.Ferry.TypedCore.Boxing.Boxing where

import Database.Ferry.TypedCore.Data.TypedCore
import Database.Ferry.TypedCore.Data.Instances()
import Database.Ferry.TypedCore.Data.Type
import Database.Ferry.Common.Data.Base

import Control.Monad.Reader
                               
import qualified Data.Map as M (lookup)
import Data.Maybe (fromJust)

-- | Execute the unboxing in the presence of type environment env
runBoxing :: TyEnv -> CoreExpr -> CoreExpr
runBoxing env = fst . flip runReader (env, Nothing, emptyEnv) . topBox

-- | An expression can be either a list or an atom.
--  a unboxed list is atom. An boxed atom becomes a list.
data Box = Atom
         | List
         | BFn Box Box
       deriving (Eq, Show)

-- | Box environment  
type BoxEnv = [(Identifier, Box)]

-- | Expected boxing value
type Context = Maybe Box

-- | Initial box environment
emptyEnv :: BoxEnv
emptyEnv = []    

-- | Boxing environment, containing type environment, context and boxing environment
type Boxing = Reader (TyEnv, Context, BoxEnv)

-- * Helper function that modify the state

-- | Store identifier i with boxing value b in the boxing environment for
-- the given boxing computation
addToEnv :: Identifier -> Box -> Boxing a -> Boxing a
addToEnv i b = local (\(gE, cE, bE) -> (gE, cE, (i, b):bE))

-- | Lookup the type scheme of an identifier in the type environment
fromGam :: Identifier -> Boxing (Maybe TyScheme)
fromGam i = do
             (g, _, _) <- ask
             return $ M.lookup i g

-- | Lookup the boxing value of an identifier in the box environment                 
fromEnv :: Identifier -> Boxing Box
fromEnv i = do
              (_, _, env) <- ask
              case lookup i env of
                  Just x -> return x
                  Nothing -> error $ "Identifier: " ++ i ++ " not found in env during boxing."

-- | Run the boxing computation with expected boxing value t
withContext :: Box -> Boxing a -> Boxing a
withContext t = local (\(gE, _, bE) -> (gE, Just t, bE)) 

-- | Get the boxing context value (or expected boxing value)
getFromContext :: Boxing (Maybe Box) 
getFromContext = do
                    (_, c, _) <- ask
                    return c

-- | Run the boxing computation without an expected boxing value                                                     
noContext :: Boxing a -> Boxing a
noContext = local (\(gE, _, bE) -> (gE, Nothing, bE))

-- | Convert a type into a boxing value 
trans :: FType -> Box
trans (FList _) = List
trans (FFn t1 t2) = BFn (trans t1) (trans t2)
trans _           = Atom
    
{-|
Heavily simplified inst, it doesn't work as a proper inst
 from the type system it only works for types that are passed to trans.
-}
inst :: TyScheme -> FType
inst (Forall _ _ (_ :=> t)) = t

{-| 
Determine how to transform an expression given an expected box value
and the box value of the expression. 
-}
boxOp :: Box -> Box -> CoreExpr -> CoreExpr
boxOp Atom List = unboxFn
boxOp List Atom = boxFn
boxOp _ _ = id

{-| Wrap the given expression in a box-function call -}
boxFn :: CoreExpr -> CoreExpr
boxFn e = App t (Var t' "box") $ ParExpr t e
  where 
    t@(q :=> ty) = typeOf e
    t' = q :=> ty .-> ty

{-| Wrap the given expression in an unbox-function call -}
unboxFn :: CoreExpr -> CoreExpr
unboxFn e = App t (Var t' "unBox") $ ParExpr t e
  where 
    t@(q :=> ty) = typeOf e
    t' = q :=> ty .-> ty 

{-| Check whether the expected box value (if specified) matches the inferred box value-}
resultCheck :: (CoreExpr, Box) -> Boxing (CoreExpr, Box)
resultCheck (e, psi) = do
                        psir <- getFromContext 
                        case (psir, psi) of
                            (Just p, psi') | p == psi' -> return (e, psi') 
                                           | otherwise -> error $ "Expected box sort doesn't match inferred sort in expression: " ++ show e
                            (Nothing, psi') -> return (e, psi')

-- * The actual boxing process


-- | Deal with corner case of lazy unboxing, the result 
-- type is a list but the boxing says it's an atom
-- in that case we perform the unboxing
topBox :: CoreExpr -> Boxing (CoreExpr, Box)
topBox e = do 
            (e', psi) <- box e
            let t = typeOf e'
            case t of
                (_ :=> (FList _)) -> return (boxOp psi List e', List)
                _                 -> return (e', psi)

-- | Run the boxing transformation over an expression, computing
-- for every epression a new expression and its box value
box :: CoreExpr -> Boxing (CoreExpr, Box)
-- A constant is simply an atom
box c@(Constant _ _) = resultCheck (c, Atom)
-- Nil is an empty list and thus box value list
box n@(Nil _)        = resultCheck (n, List)
-- Const adds an element (an atom, a nested list has to be unboxed!) to another list
-- There are no expectations on the box value of its children.
box (Cons t e1 e2)   = do
                         (e1', psi) <- noContext $ box e1
                         (e2', psi2) <- noContext $ box e2 
                         resultCheck (Cons t (boxOp psi Atom e1') (boxOp psi2 List e2'), List)
-- Element from a list is an atom (even it is a list, it is still unboxed)
box (Elem t e s) = do
                      (e', psi) <- noContext $ box e
                      resultCheck (Elem t (boxOp psi Atom e') s, Atom)
-- A table is a list of tuples and thus has box value list
box t@(Table _ _ _ _) = resultCheck (t, List)
-- There are no expectations on the context of the conditional,
-- the branches however have to meet the expectations of the entire
-- if then else construction
box (If t e1 e2 e3) = do
                        (e1', psi1) <- noContext $ box e1
                        (e2', psi2) <- box e2
                        (e3', psi3) <- box e3
                        if psi2 == psi3
                            then resultCheck (If t (boxOp psi1 Atom e1') e2' e3', psi3)
                            else resultCheck (If t (boxOp psi1 Atom e1') (boxOp psi2 Atom e2') (boxOp psi3 Atom e3'), Atom) 
-- The bound expression doesn't have a context, the context of the whole let is equal to the context of e2
box (Let t s e1 e2) = do
                        (e1', psi1) <- noContext $ box e1
                        (e2', psi2) <- addToEnv s psi1 $ box e2
                        resultCheck (Let t s e1' e2', psi2)
-- A variable has a type in the environment if it is a global variable
-- it's box value can be retrieved from its type.
-- Otherwise the variable has to have a box value in the box environment
box (Var t x) = do 
                  ty <- fromGam x
                  case ty of
                      Nothing -> do
                                  psi <- fromEnv x
                                  resultCheck (Var t x, psi)
                      (Just t') -> do
                                   let psi = trans $ inst t' 
                                   resultCheck (Var t x, psi)
-- A record is an atom 
box (Rec t els) = do 
                    els' <- mapM (noContext . boxRec) els
                    return (Rec t els', Atom)
-- Function application
-- There are no expectations about the box value of e1
-- It will however give a box value from box value to box value.
-- The expected box value of the argument is equal to the box value
-- at the argument position of the box function type.
-- The result of the application is equal to the box value of result box value
-- in the inferred box function type.
box (App t e1 e2) = do
                     (e1', psi) <- noContext $ box e1
                     let (psia, psir ) = case psi of
                                           (BFn psia' psir') -> (psia', psir')
                                           _               -> error $ show psi ++ "not a function box"   
                     (e2', _psi2) <- withContext psia $ boxParam e2
                     resultCheck (App t e1' e2', psir)
-- Similar to app
box (BinOp t (Op o) e1 e2) = do
                               ty <- fromGam o
                               case ty of
                                   Nothing -> error $ "Non primitive operator during boxing phase, this should not happen: " ++ show o
                                   (Just t') -> do
                                                let (BFn psi1 (BFn psi2 psi3)) = trans $ inst t'
                                                (e1', psi1') <- noContext $ box e1
                                                (e2', psi2') <- noContext $ box e2
                                                resultCheck (BinOp t (Op o) (boxOp psi1' psi1 e1') (boxOp psi2' psi2 e2'), psi3)

-- Box the expression in a record element
-- They should be of box type atom, a list needs to be unboxed.
boxRec :: RecElem -> Boxing RecElem 
boxRec (RecElem t x e) = do
                          (e', psi) <- box e
                          return $ RecElem t x (boxOp psi Atom e')    

-- | Box function parameters
boxParam :: Param -> Boxing (Param, Box)   
boxParam (ParExpr t e) = do
                           (e', psi) <- noContext $ box e
                           psie <- getFromContext
                           return $ (ParExpr t $ boxOp psi (fromJust psie) e', fromJust psie)
boxParam (ParAbstr t p e) = do
                             let args = p
                             psie <- getFromContext
                             let (asso, boxR) = varsWithBox args $ fromJust psie
                             (e', psi) <- foldr (\(v, t') r -> addToEnv v t' r) (noContext $ box e) asso
                             return (ParAbstr t p (boxOp psi boxR e'), boxR)
 
{-
-- | Retrieve the variables in a pattern    
getVars :: Pattern -> [String]
getVars (PVar v) = [v]
getVars (Pattern p) = p  
-}

-- | Construct a list of all function arguments with their respective box value, and the result box value of the function.
varsWithBox :: [String] -> Box -> ([(String, Box)], Box)
varsWithBox []             b = ([], b)
varsWithBox (x:xs) (BFn b1 b2) = (\(l, b) -> ((x, b1):l, b)) (varsWithBox xs b2)
varsWithBox _      _           = error $ "varswithBox err, should not happen"   