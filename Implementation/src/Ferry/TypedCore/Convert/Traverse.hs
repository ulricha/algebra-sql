module Ferry.TypedCore.Convert.Traverse where
    
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.Front.Data.Base


data FoldCore b p r = FoldCore {binOpF :: Qual FType -> Op -> b -> b -> b
                             ,unaOpF :: Qual FType -> Op -> b -> b
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

idFoldCore :: FoldCore CoreExpr Param RecElem
idFoldCore = FoldCore BinOp UnaOp Constant Var App Let Rec Cons Nil Elem Table If ParExpr ParAbstr RecElem  

-- | This function traverses the whole CoreExpr tree and applies the given function at every node after
-- | that the function is applied to all its children.
traverse :: (FoldCore b p r) -> CoreExpr -> b
traverse f (BinOp t o e1 e2)              = (binOpF f) t o (traverse f e1) $ traverse f e2
traverse f (UnaOp t o e1)                 = (unaOpF f) t o $ traverse f e1
traverse f (Constant t c)                 = (constantF f) t c
traverse f (Var t s)                      = (varF f) t s
traverse f (App t e1 (ParExpr t2 e2))     = (appF f) t (traverse f e1) $ (pExprF f) t2 $ traverse f e2
traverse f (App t e1 (ParAbstr t2 p e))   = (appF f) t (traverse f e1) $ (pAbstrF f) t2 p $ traverse f e
traverse f (Let t s e1 e2)                = (letF f) t s (traverse f e1) $ traverse f e2
traverse f (Rec t els)                    = (recF f) t $ map (\(RecElem t s e) -> (rRecEF f) t s $ traverse f e) els
traverse f (Cons t e1 e2)                 = (consF f) t (traverse f e1) $ traverse f e2
traverse f (Nil t)                        = (nilF f) t
traverse f (Elem t e s)                   = (elemF f) t (traverse f e) s
traverse f (If t e1 e2 e3)                = (ifF f) t (traverse f e1) (traverse f e2) $ traverse f e3
traverse f (Table t s c k)                = (tableF f) t s c k
