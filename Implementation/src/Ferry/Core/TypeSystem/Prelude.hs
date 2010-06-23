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
             [("+", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("-", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("*", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("/", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("%", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("^", Forall 1 0 $ [IsIn "Num" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) (FGen 1)))
             ,("==", Forall 1 0 $ [IsIn "Eq" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("!=", Forall 1 0 $ [IsIn "Eq" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("<=", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("<", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,(">", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("not", Forall 0 0 $ [] :=> bool .-> bool)
             ,("and", Forall 0 0 $ [] :=> bool .-> bool .-> bool)
             ,("or", Forall 0 0 $ [] :=> bool .-> bool .-> bool)
             ,("map", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> list (genT 1) .-> list (genT 2))
             ,("concatMap", Forall 2 0 $ [] :=> (genT 1 .-> list (genT 2)) .-> list (genT 1) .-> list (genT 2))
             ,("single", Forall 1 0 $ [] :=> (list $ genT 1) .-> genT 1)
             ,("filter", Forall 1 0 $ [] :=> (genT 1 .-> bool) .-> (list $ genT 1) .-> (list $ genT 1))
             ,("lookup", Forall 2 0 $ [IsIn "Eq" (genT 1)] :=> list (rec [(RLabel "1", genT 1), (RLabel "2", genT 2)]) .-> genT 1 .-> genT 2)
             ,("groupByN", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ FTF Tr (genT 2)))
             ,("orderBy", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("orderByDescending", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("thenBy", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("thenByDescending", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             -- ,("concatMap", Forall 2 $ [] :=> (genT 1 .-> (list $ genT 2)) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("concatMap'", Forall 3 0 $ [] :=> (genT 1 .-> (list $ genT 2)) .-> (genT 1 .-> genT 2 .-> genT 3) .-> (list $ genT 1) .-> (list $ genT 3))
             ,("groupWith", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> (list $ genT 1) .-> (list $ rec [(RLabel "1", genT 3), (RLabel "2", list $ genT 2)]))
             -- ,("groupWithN", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> (list $ genT 1) .-> (list $ rec [(RLabel "1", genT 3), (RLabel "2", list $ genT 2)]))
             ]
