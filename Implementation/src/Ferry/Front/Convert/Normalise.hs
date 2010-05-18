{-# LANGUAGE TypeSynonymInstances#-}
module Ferry.Front.Convert.Normalise where

import Ferry.Front.Data.Language
import Ferry.Front.Data.Base
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Instances
import Ferry.Compiler.Error.Error

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative (Applicative(..), (<**>), (<$>), (<*>))

-- Everything we normalise is member of the normalise class
class Normalise a where
    normalise :: a -> Transformation a
    
-- Substitutions are stored in a map, a variable name is always substituted by a new one
type Substitution = M.Map Identifier Identifier

-- The transformation monad
-- The outcome is a pair of the outcome or an error and a state.
-- The state contains substitutions and a supply of fresh variables
type Transformation = ErrorT FerryError (State (Int, Substitution))

instance Applicative Transformation where
    (<*>) = ap
    pure = return
    
{- infixl 4 <->
(<->) :: Transformation (a -> b) -> Transformation a -> Transformation b
e1 <-> e2 = do
              (_, m) <- get
              e1' <- e1
              (s, _) <- get
              put (s, m)
              e2' <- e2
              put (s, m)
              return $ e1' e2' -}
              
restoreState :: Transformation a -> Transformation a
restoreState e = do
                    (_, s) <- get
                    e' <- e
                    (c, _) <- get
                    put (c, s)
                    return e'
    
-- Convenience function for operations on the monad

-- Add a substitution
addSubstitution :: Identifier -> Identifier -> Transformation ()
addSubstitution i n = do
                        modify (\(v, m) ->  (v, M.insert i n m))

-- Apply a substitution to the given identifier
applySubstitution :: Identifier -> Transformation Identifier
applySubstitution i = do
                        (_,m) <- get
                        case M.lookup i m of
                            Nothing -> return i
                            Just i' -> return i'

-- Remove the list of given identifiers from the substitution list
removeSubstitution :: [Identifier] -> Transformation ()
removeSubstitution is = do
                         modify (\(v, m) -> (v, foldr M.delete m is))

-- Retrieve a fresh variable name from the monad, the state is updated accordingly                         
getFreshIdentifier :: Transformation String
getFreshIdentifier = do
                        (n, subst) <- get
                        put (n + 1, subst)
                        return $ "_v" ++ show n

-- Retrieve a fresh variable from the monad                        
getFreshVariable :: Transformation Expr
getFreshVariable = do
                    n <- getFreshIdentifier
                    return $ Var (Meta emptyPos) n

-- Transform the given expression
runTransformation :: Normalise a => a -> Either FerryError a
runTransformation x = fst $ flip runState (0, M.empty) $ runErrorT $ (normalise x)

-- Instances of the normalise class

instance Normalise Arg where
    normalise (AExpr m e) = do
                             e' <- normalise e
                             return $ AExpr m e'
    normalise (AAbstr m p e) = restoreState $
                                do
                                 let vs = vars p
                                 removeSubstitution vs
                                 e' <- normalise e
                                 return $ AAbstr m p e'

instance Normalise RecElem where
    normalise r@(TrueRec m s e) = 
                                case (s, e) of
                                 (Right i, Just ex) -> do
                                                        e' <- normalise ex
                                                        return $ TrueRec m s $ Just e'
                                 (Right i, Nothing) -> do
                                                        return $ TrueRec m s $ Just (Var m i)
                                 (Left i, Nothing) -> case i of
                                                        (Elem m e (Left x)) -> do
                                                                                 i' <- normalise i
                                                                                 return $ TrueRec m (Right x) $ Just i'
                                                        (_)                  ->  throwError $ IllegalRecSyntax r
                                 (_) -> throwError $ IllegalRecSyntax r
    normalise (TuplRec m i e) = do
                                 e' <- normalise e
                                 return $ TuplRec m i e'

instance Normalise Binding where
    normalise (Binding m s e) = do
                                 e' <- normalise e
                                 removeSubstitution [s]
                                 return $ Binding m s e'

instance Normalise Expr where
    normalise c@(Const _ _) = return c
    normalise (UnOp m o e) = UnOp m o <$> normalise e
    normalise (BinOp m o e1 e2) = BinOp m o <$> normalise e1 <*> normalise e2
    normalise (Var m i) = do
                            i' <- applySubstitution i
                            return $ Var m i'
    normalise (App m e a) = do
                             e' <- normalise e
                             a' <- mapM normalise a
                             return $ App m e' a'
    normalise (If m e1 e2 e3) =  If m <$> normalise e1 <*> normalise e2 <*> normalise e3
    normalise (Record  m els) = case (head els) of
                                 (TrueRec _ _ _) ->
                                        do
                                          els' <- mapM normalise els
                                          return $ Record m $ L.sortBy sortElem els'
                                 (TuplRec _ _ _) ->
                                        do
                                          els' <- mapM normalise els
                                          return $ Record m els'
    normalise (Paren _ e) = normalise e
    normalise (List m es) = do
                             es' <- mapM normalise es
                             return $ List m es'
    normalise (Elem m e i) = do
                              e' <- normalise e
                              return $ Elem m e' i
    normalise (Lookup m e1 e2) = normalise $ App m (Var m "lookup") [AExpr (getMeta e2) e2, AExpr (getMeta e1) e1]
    normalise (Let m bs e) = case bs of
                                [b] -> do
                                        (_, s) <- get
                                        b' <- normalise b
                                        e' <- normalise e
                                        (c, _) <- get
                                        put (c, s)
                                        return $ Let m [b'] e'
                                (b:bss) -> do
                                            b' <- normalise b
                                            let nb = Let m bss e
                                            tl <- normalise nb
                                            return $ Let m [b'] tl
    normalise (Table m s cs ks) = return $ Table m s (L.sortBy sortColumn cs) ks
    normalise (Relationship m c1 e1 c2 e2 k1 k2) =
                    case (c1, c2) of
                        (One _, One _) -> undefined
    
    
sortColumn :: Column -> Column -> Ordering
sortColumn (Column _ s1 _) (Column _ s2 _) = compare s1 s2

sortElem :: RecElem -> RecElem -> Ordering
sortElem (TrueRec _ (Right i1) _) (TrueRec _ (Right i2) _) = compare i1 i2
sortElem _                        _                        = error "illegal input to sortElem Normalise.hs"




{-
Table        :: Meta -> String -> [Column] -> [Key] -> Expr
Relationship :: Meta -> Cardinality -> Expr -> Cardinality -> Expr -> Key -> Key -> Expr
QComp        :: Meta -> QCompr -> Expr
-}
