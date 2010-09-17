{-# LANGUAGE TemplateHaskell #-}
module Ferry.TypeSystem.Prelude where

import Ferry.Impossible    
import Ferry.TypedCore.Data.TypeClasses
import Ferry.TypedCore.Data.Type

import qualified Data.Map as M

baseEnv :: ClassEnv
baseEnv = case addAll emptyClassEnv of
            Right a -> a
            _       -> $impossible
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
             ,(">=", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("<", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,(">", Forall 1 0 $ [IsIn "Ord" (FGen 1)] :=> FFn (FGen 1) (FFn (FGen 1) bool))
             ,("not", Forall 0 0 $ [] :=> bool .-> bool)
             ,("and", Forall 0 0 $ [] :=> list bool .-> bool)
             ,("or", Forall 0 0 $ [] :=> list bool .-> bool)
             ,("&&", Forall 0 0 $ [] :=> bool .-> bool .-> bool)
             ,("||", Forall 0 0 $ [] :=> bool .-> bool .-> bool)
             ,("minP", Forall 1 0 $ [] :=> (list $ genT 1) .-> (list $ genT 1) .-> FInt)
             ,("count", Forall 1 0 $ [] :=> (list $ genT 1) .-> FInt)
             ,("all", Forall 0 0 $ [] :=> (list bool) .-> bool)
             ,("map", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> list (genT 1) .-> list (genT 2))
             ,("concatMap", Forall 2 0 $ [] :=> (genT 1 .-> list (genT 2)) .-> list (genT 1) .-> list (genT 2))
             ,("concat", Forall 1 0 $ [] :=> (list $ list $ genT 1) .-> (list $ genT 1))
             ,("single", Forall 1 0 $ [] :=> (list $ genT 1) .-> genT 1)
             ,("filter", Forall 1 0 $ [] :=> (genT 1 .-> bool) .-> (list $ genT 1) .-> (list $ genT 1))
             ,("lookup", Forall 2 0 $ [IsIn "Eq" (genT 1)] :=> list (rec [(RLabel "1", genT 1), (RLabel "2", genT 2)]) .-> genT 1 .-> genT 2)
             ,("length", Forall 1 0 $ [] :=> (list $ genT 1) .-> FInt)
             ,("fst", Forall 2 0 $ [] :=> rec [(RLabel "1", genT 1), (RLabel "2", genT 2)] .-> genT 1)
             ,("snd", Forall 2 0 $ [] :=> rec [(RLabel "1", genT 1), (RLabel "2", genT 2)] .-> genT 2)
             ,("the", Forall 1 0 $ [] :=> (list $ genT 1) .-> genT 1)
             ,("head", Forall 1 0 $ [] :=> (list $ genT 1) .-> genT 1)
             ,("tail", Forall 1 0 $ [] :=> (list $ genT 1) .-> (list $ genT 1))
             ,("nub", Forall 1 0 $ [] :=> (list $ genT 1) .-> (list $ genT 1))
             ,("groupByN", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ FTF Tr (genT 2)))
             ,("groupBy'", Forall 3 1 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ list $ genT 2))
             ,("groupBy1", Forall 3 1 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2)]))
             ,("groupBy2", Forall 4 2 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2), (RGen 2, genT 4)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2), (RGen 2, list $ genT 4)]))
             ,("groupBy3", Forall 5 3 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2), (RGen 2, genT 4), (RGen 3, genT 5)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2), (RGen 2, list $ genT 4), (RGen 3, list $ genT 5)]))
             ,("groupBy4", Forall 6 4 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2), (RGen 2, genT 4), (RGen 3, genT 5), (RGen 4, genT 6)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2), (RGen 2, list $ genT 4), (RGen 3, list $ genT 5), (RGen 4, list $ genT 6)]))
             ,("groupBy5", Forall 7 5 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2), (RGen 2, genT 4), (RGen 3, genT 5), (RGen 4, genT 6), (RGen 5, genT 7)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2), (RGen 2, list $ genT 4), (RGen 3, list $ genT 5), (RGen 4, list $ genT 6), (RGen 5, list $ genT 7)]))
             ,("groupBy6", Forall 8 6 $ [] :=> (genT 1 .-> rec [(RGen 1 ,genT 2), (RGen 2, genT 4), (RGen 3, genT 5), (RGen 4, genT 6), (RGen 5, genT 7), (RGen 6, genT 8)]) .-> (genT 1 .-> genT 3) .-> list (genT 1) .-> (list $ rec [(RGen 1 , list $ genT 2), (RGen 2, list $ genT 4), (RGen 3, list $ genT 5), (RGen 4, list $ genT 6), (RGen 5, list $ genT 7), (RGen 6, list $ genT 8)]))
             ,("zip", Forall 2 0 $ [] :=> (list $ genT 1) .-> (list $ genT 2) .-> list (rec [(RLabel "1", genT 1),(RLabel "2", genT 2)]))
             ,("unzip", Forall 2 0 $ [] :=> (list $ rec [(RLabel "1", genT 1), (RLabel "2", genT 2)]) .-> rec [(RLabel "1", list $ genT 1), (RLabel "2", list $ genT 2)])
             ,("orderBy", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("orderByDescending", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("thenBy", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("thenByDescending", Forall 2 0 $ [] :=> (genT 1 .-> genT 2) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("concatMap", Forall 2 0 $ [] :=> (genT 1 .-> (list $ genT 2)) .-> (list $ genT 1) .-> (list $ genT 2))
             ,("concatMap'", Forall 3 0 $ [] :=> (genT 1 .-> (list $ genT 2)) .-> (genT 1 .-> genT 2 .-> genT 3) .-> (list $ genT 1) .-> (list $ genT 3))
             ,("groupWith", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> (list $ genT 1) .-> (list $ rec [(RLabel "1", genT 3), (RLabel "2", list $ genT 2)]))
             -- ,("groupWithN", Forall 3 0 $ [] :=> (genT 1 .-> genT 2) .-> (genT 1 .-> genT 3) .-> (list $ genT 1) .-> (list $ rec [(RLabel "1", genT 3), (RLabel "2", list $ genT 2)]))
             ]
