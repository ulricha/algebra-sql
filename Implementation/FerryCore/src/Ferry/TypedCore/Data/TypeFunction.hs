{-| Handle the type functions related to records-}
module Ferry.TypedCore.Data.TypeFunction where

import Ferry.TypedCore.Data.Type

-- | Evaluate the type function application to a type that doesn't contain a function    
evalTy :: FType -> FType
evalTy o@(FTF fn' t) = case applyTyFn fn' t of
                        Right t' -> evalTy t'
                        Left _  -> o
evalTy (FList t) = FList $ evalTy t
evalTy (FRec t) = FRec $ map (\(i,t') -> (i, evalTy t')) t
evalTy (FFn t1 t2) = FFn (evalTy t1) $ evalTy t2
evalTy t = t

-- | Apply a type function to a type
applyTyFn :: FTFn -> FType -> Either FType FType
applyTyFn Tr (FList t) = Right $ FList $ FList t
applyTyFn Tr (FRec ts) = Right $ FRec $ map (\(l, t) -> (l, FList t)) ts 
applyTyFn Tr (FTF Tr' t) = Right $ t
applyTyFn Tr (FTF Tr t) = case applyTyFn Tr t of
                            Right t' -> applyTyFn Tr t'
                            Left t' -> Left $ FTF Tr t'
applyTyFn Tr (FVar v) = Left $ FTF Tr $ FVar v
applyTyFn Tr t        = Left $ FTF Tr t
applyTyFn Tr' (FList (FList t)) = Right $ FList t
applyTyFn Tr' (FRec ts) = Right $ FRec $ map (\(l, t) -> (l, case t of 
                                                              FList t' -> t'
                                                              _       -> error "Not a list")) ts
applyTyFn Tr' (FTF Tr t) = Right t 
applyTyFn Tr' _ = error "Cannot apply Tr'"