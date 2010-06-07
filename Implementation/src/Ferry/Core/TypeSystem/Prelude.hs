module Ferry.Core.TypeSystem.Prelude where
    
import Ferry.TypedCore.Data.TypeClasses
import Ferry.TypedCore.Data.Type

import qualified Data.Map as M

baseEnv :: ClassEnv
baseEnv = case addAll emptyClassEnv of
            Right a -> a
    where
        addAll =
                do
                    addBaseClasses <:> addBaseInstances
        addBaseClasses =
         addClass "Eq" []
         <:> addClass "Num" []
         <:> addClass "Ord" ["Eq"]
        addBaseInstances =
         addInstance [] (IsIn "Eq" FInt)
         <:> addInstance [] (IsIn "Eq" FFloat)
         <:> addInstance [] (IsIn "Eq" FBool)
         <:> addInstance [] (IsIn "Eq" FString)
         <:> addInstance [IsIn "Eq" $ FVar "a"] (IsIn "Eq" $ FList $ FVar "a")
         <:> addInstance [] (IsIn "Num" FFloat)
         <:> addInstance [] (IsIn "Num" FInt)
         <:> addInstance [] (IsIn "Ord" FFloat)
         <:> addInstance [] (IsIn "Ord" FBool)
         <:> addInstance [] (IsIn "Ord" FString)
         <:> addInstance [IsIn "Ord" $ FVar "a"] (IsIn "Ord" $ FList $ FVar "a")
         
primitives :: TyEnv
primitives = M.fromList $
             [("+", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("-", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("*", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("/", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("%", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("^", Forall 1 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))]


