{-# LANGUAGE TypeSynonymInstances#-}
module Ferry.Front.Convert.FrontToCore where
    
import Ferry.Front.Data.Language
import qualified Ferry.Core.Data.Core as C

import Ferry.Compiler.Error.Error

import Control.Monad.State
import Control.Monad.Error
import Control.Applicative (Applicative(..), (<**>), (<$>), (<*>))

type Transformation = ErrorT FerryError (State Int)

instance Applicative Transformation where
    (<*>) = ap
    pure = return
    
getFreshIdentifier :: Transformation String
getFreshIdentifier = do
                        n <- get
                        put $ n + 1
                        return $ "_v" ++ show n
                        
runTransformation :: Expr -> Either FerryError C.CoreExpr
runTransformation x = fst $ flip runState 0 $ runErrorT $ toCore x

toCore :: Expr -> Transformation C.CoreExpr
toCore (UnOp         _ o e1)     = C.UnaOp <$> opToCore o <*> toCore e1
toCore (BinOp        _ o e1 e2)  = C.BinOp <$> opToCore o <*> toCore e1 <*> toCore e2
toCore (Const        _ c)        = C.Constant <$> pure c
toCore (Var          _ i)        = C.Var <$> pure i
toCore (App          _ e args)   = foldl C.App <$> toCore e <*> mapM argToCore args
toCore (If           _ e1 e2 e3) = C.If <$> toCore e1 <*> toCore e2 <*> toCore e3
toCore (Record       _ rs)       = C.Rec <$> mapM recToCore rs
toCore (Paren        _ e)        = toCore e
toCore (List         _ es)       = foldr C.Cons C.Nil <$> mapM toCore es
toCore (Elem         _ e esi)    = C.Elem <$> toCore e <*> varIntToCore esi
toCore e@(Lookup       _ _ _)    = throwError $ FrontToCoreError "Lookup not supported by core" e
toCore er@(Let          _ bs e)  = case bs of
                                     [(Binding _ s eb)] -> C.Let <$> pure s <*> toCore eb <*> toCore e
                                     _                  -> throwError $ FrontToCoreError "Let with multiple bindings" er
toCore (Table        _ s cs ks)  = C.Table <$> pure s <*> mapM colToCore cs <*> mapM keyToCore ks
toCore e@(Relationship _ _ _ _ _ _ _) = throwError $ FrontToCoreError "Relationship not supported by core" e
toCore e@(QComp        _ _)       = throwError $ FrontToCoreError "List comprehensions not supported by core" e
 
opToCore :: Op -> Transformation C.Op
opToCore (Op _ s) = C.Op <$> pure s

argToCore :: Arg -> Transformation C.Param
argToCore (AExpr _ e) = C.ParExpr <$> toCore e
argToCore a@(AAbstr _ ps e) = case ps of 
                             [p] -> C.ParAbstr <$> patToCore p <*> toCore e
                             _   -> throwError $ FrontToCoreArgError "Abstraction only accepts single patterns" a

recToCore :: RecElem -> Transformation C.RecElem
recToCore r@(TrueRec _ i e) = case (i, e) of
                             (Right s, Just e) -> C.RecElem <$> pure s <*> toCore e
                             _                 -> throwError $ FrontToCoreRecError "Record element of wrong form" r
recToCore (TuplRec _ i e)   = C.RecElem <$> (pure $ show i) <*> toCore e

varIntToCore :: Either String Integer -> Transformation String
varIntToCore (Left s) = pure s
varIntToCore (Right i) = pure $ show i

colToCore :: Column -> Transformation C.Column
colToCore (Column _ s t) = C.Column <$> pure s <*> typeToCore t

keyToCore :: Key -> Transformation C.Key
keyToCore (Key _ ks) = C.Key <$> pure ks

patToCore :: Pattern -> Transformation C.Pattern
patToCore (PVar _ s) = C.PVar <$> pure s
patToCore (PPat _ vs) = C.Pattern <$> pure vs

typeToCore :: Type -> Transformation C.Type
typeToCore (TInt    _) = pure C.TInt
typeToCore (TFloat  _) = pure C.TFloat
typeToCore (TString _) = pure C.TString
typeToCore (TBool   _) = pure C.TBool
