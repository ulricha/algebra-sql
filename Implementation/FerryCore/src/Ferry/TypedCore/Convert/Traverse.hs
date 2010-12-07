{- | Provides a traverse method that given functions for parts of the AST traverses the AST and applies the function where appropriate -}
module Ferry.TypedCore.Convert.Traverse where
    
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.Common.Data.Base
import Control.Monad

-- | Datatype that contains the functions that are needed for a traversal
data FoldCore b p r = FoldCore {binOpF :: Qual FType -> Op -> b -> b -> b
                             ,constantF :: Qual FType -> Const -> b
                             ,varF :: Qual FType -> String -> b
                             ,appF :: Qual FType -> b -> p -> b
                             ,letF :: Qual FType -> String -> b -> b -> b
                             ,recF :: Qual FType -> [r] -> b
                             ,consF :: Qual FType -> b -> b -> b
                             ,nilF :: Qual FType -> b
                             ,elemF :: Qual FType -> b -> String -> b
                             ,tableF :: Qual FType -> String -> [Column] -> [Key] -> b
                             ,ifF :: Qual FType -> b -> b -> b -> b
                             ,pExprF :: Qual FType -> b -> p
                             ,pAbstrF :: Qual FType -> Pattern -> b -> p
                             ,rRecEF :: Qual FType -> String -> b -> r}

-- | Identity traversel
idFoldCore :: FoldCore CoreExpr Param RecElem
idFoldCore = FoldCore BinOp Constant Var App Let Rec Cons Nil Elem Table If ParExpr ParAbstr RecElem  

-- | Monadic traversal
mFoldCore :: Monad m => FoldCore (m CoreExpr) (m Param) (m RecElem)
mFoldCore = FoldCore (\t o -> liftM2 (BinOp t o))
                      (\t c -> return $ Constant t c)
                      (\t s -> return $ Var t s)
                      (\t -> liftM2 $ App t)
                      (\t s -> liftM2 $ Let t s)
                      (\t rs -> do
                                 rs' <- sequence rs
                                 return $ Rec t rs')
                      (\t -> liftM2 $ Cons t)
                      (\t -> return $ Nil t)
                      (\t e s -> do
                                  e' <- e
                                  return $ Elem t e' s)
                      (\t s c k -> return $ Table t s c k)
                      (\t -> liftM3 $ If t)
                      (\t -> liftM $ ParExpr t)
                      (\t p -> liftM $ ParAbstr t p)
                      (\t s -> liftM $ RecElem t s)
                     

-- | This function traverses the whole CoreExpr tree and applies the given function at every node after
-- | that the function is applied to all its children.
traverse :: (FoldCore b p r) -> CoreExpr -> b
traverse f (BinOp t o e1 e2)              = (binOpF f) t o (traverse f e1) $ traverse f e2
traverse f (Constant t c)                 = (constantF f) t c
traverse f (Var t s)                      = (varF f) t s
traverse f (App t e1 (ParExpr t2 e2))     = (appF f) t (traverse f e1) $ (pExprF f) t2 $ traverse f e2
traverse f (App t e1 (ParAbstr t2 p e))   = (appF f) t (traverse f e1) $ (pAbstrF f) t2 p $ traverse f e
traverse f (Let t s e1 e2)                = (letF f) t s (traverse f e1) $ traverse f e2
traverse f (Rec t els)                    = (recF f) t $ map (\(RecElem t' s e) -> (rRecEF f) t' s $ traverse f e) els
traverse f (Cons t e1 e2)                 = (consF f) t (traverse f e1) $ traverse f e2
traverse f (Nil t)                        = (nilF f) t
traverse f (Elem t e s)                   = (elemF f) t (traverse f e) s
traverse f (If t e1 e2 e3)                = (ifF f) t (traverse f e1) (traverse f e2) $ traverse f e3
traverse f (Table t s c k)                = (tableF f) t s c k
