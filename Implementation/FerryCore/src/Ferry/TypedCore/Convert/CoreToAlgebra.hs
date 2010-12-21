{-# LANGUAGE TemplateHaskell #-}
{- |
This module transforms typed ferry core into a relational algebra DAG.
The transformation assumes that given programs are type correct and some
functions on lists have been inlined (transformations performed by RewriteStage).

For a more complete overview see:

http://www-db.informatik.uni-tuebingen.de/files/publications/avalanche-safe-linq.pdf
-}
module Ferry.TypedCore.Convert.CoreToAlgebra where


import Ferry.Impossible
import Ferry.Common.Data.Base

import Ferry.Algebra

import Ferry.TypedCore.Data.Type (Qual (..), FType (..), RLabel (..), isPrim)
import Ferry.TypedCore.Data.TypedCore as T

import qualified Data.Map as M 
import qualified Data.List as L
import Data.Maybe (fromJust, isJust)

-- | Section introducing aliases for commonly used columns

-- | Results are stored in column:
resCol, ordCol, ordPrime, iterPrime, iterR, posPrime, posPrimePrime, outer, inner, oldCol :: String
resCol    = "item99999001"
ordCol    = "item99999801"
ordPrime  = "item99999804"
iterPrime = "item99999701"
iterR     = "item99999703"
posPrime  = "item99999601"
posPrimePrime = "item99999602"
outer     = "item99999501"
inner     = "item99999401"
oldCol    = "item99999301"

-- | Construct the ith item columns
mkPrefixCol :: Int -> String
mkPrefixCol i = "item" ++ prefixCol ++ (show i)

-- | Construct the ith iter column
mkPrefixIter :: Int -> String
mkPrefixIter i = "iter" ++ prefixCol ++ (show i)

-- | Prefix for intermediate column numbers
prefixCol :: String
prefixCol = "9999"

-- | Transform Ferry core into a relation algebra modelled as a DAG
coreToAlgebra :: CoreExpr -> GraphM AlgRes
-- | Primitive values
coreToAlgebra (Constant _ CUnit) = do
                                    n1 <- attach "pos" natT (nat 1) 
                                            =<< attach "item1" intT (int 0) 
                                            =<< getLoop
                                    return (n1, [Col 1 AInt], emptyPlan)
coreToAlgebra (Constant _ (CInt i)) = do 
                                        n1 <- attach "pos" natT (nat 1) 
                                                =<< attach "item1" intT (int i) 
                                                =<< getLoop
                                        return (n1, [Col 1 AInt], emptyPlan)
coreToAlgebra (Constant _ (CBool i)) = do
                                         n1 <- attach "pos" natT (nat 1) 
                                                =<< attach "item1" boolT (bool i) 
                                                =<< getLoop
                                         return (n1, [Col 1 ABool], emptyPlan)
coreToAlgebra (Constant _ (CFloat i)) = do
                                         n1 <- attach "pos" natT (nat 1) 
                                            =<< attach "item1" doubleT (double i) 
                                            =<< getLoop
                                         return (n1, [Col 1 ADouble], emptyPlan)
coreToAlgebra (Constant _ (CString i)) = do
                                          n1 <- attach "pos" natT (nat 1) 
                                                =<< attach "item1" stringT (string i) 
                                                =<< getLoop
                                          return (n1, [Col 1 AStr], emptyPlan)
-- Binary operators
coreToAlgebra (BinOp (_ :=> t) (Op o) e1 e2) = do
                                         (q1, [Col 1 _t1], _m1) <- coreToAlgebra e1
                                         (q2, [Col 1 _t2], _m2) <- coreToAlgebra e2
                                         n1 <- proj [("iter", "iter"), ("pos", "pos"), ("item1", resCol)] 
                                                =<< oper o resCol  "item1" (mkPrefixCol 1) 
                                                =<< eqJoin "iter" (mkPrefixIter 1) q1 
                                                =<< proj [(mkPrefixIter 1, "iter"), (mkPrefixCol 1, "item1")] q2
                                         return (n1, fst $ typeToCols t 1, emptyPlan)
-- Let bindings
coreToAlgebra (Let _ s e1 e2) = do
                                    (q1, cs1, m1) <- coreToAlgebra e1
                                    withBinding s (q1, cs1, m1) $ coreToAlgebra e2
-- Variable lookup
coreToAlgebra (Var _ n) = fromGam n
-- Record construction, body of the rule can be found in recElemsToAlgebra
coreToAlgebra (Rec _ (e:els)) = foldl recElemsToAlgebra (recElemToAlgebra e) els
coreToAlgebra (Rec _ []) = $impossible
-- Record element access.
coreToAlgebra (Elem _ e n) = do
                                (q1, cs1 ,(SubPlan ts1)) <- coreToAlgebra e
                                let csn = getCol n cs1
                                let (csn', i) = decrCols csn
                                let ln = leafNumbers csn'
                                let ts = SubPlan $ M.fromList [ (l, fromJust r) | l <- ln, let r = M.lookup (l+i) ts1, isJust r]
                                let projPairs = zip (leafNames csn') (leafNames csn)
                                n1 <- proj (("iter", "iter"):("pos", "pos"):projPairs) q1
                                return (n1, csn', ts)
--Empty lists
coreToAlgebra (Nil (_ :=> (FList t))) = do
                                 let cs = fst $ typeToCols t 1
                                 let schema = ("iter", natT):("pos", natT):(colsToSchema cs)
                                 n1 <- emptyTable schema
                                 return (n1, cs, emptyPlan)
coreToAlgebra (Nil _) = $impossible -- After type checking the only thing that reaches this stage has a list type
-- List constructor, because of optimisation chances contents has been directed to special functions
coreToAlgebra (c@(Cons _ _ _)) = listFirst c
-- Database tables
coreToAlgebra (Table _ n cs ks) = do
                                    let cs' = coreCol2AlgCol cs
                                    let keys' = key2Key cs' ks
                                    loop <- getLoop
                                    n1 <- cross loop 
                                            =<< rank "pos" (map (\ki -> (ki, Asc)) $ head keys') 
                                            =<< dbTable n cs' keys'
                                    return (n1, cs', emptyPlan)
-- If then else
coreToAlgebra (If _ e1 e2 e3) = do
                                  (q1, _cs1, _ts1) <- coreToAlgebra e1
                                  -- Get current gamma
                                  gam <- getGamma
                                  -- Build loop and gamma for then branch 
                                  loopThen <- proj [("iter", "iter")] =<< select "item1" q1
                                  gamThen <- transformGam algResLoop loopThen gam 
                                  --Evaluate then branch
                                  (q2, cs2, ts2) <- withContext gamThen loopThen $ coreToAlgebra e2
                                  -- Build loop and gamma for else branch
                                  loopElse <- proj [("iter", "iter")] 
                                                =<< select resCol 
                                                =<< notC resCol "item1" q1
                                  gamElse <- transformGam algResLoop loopElse gam 
                                  --Evaluate else branch
                                  (q3, _cs3, ts3) <- withContext gamElse loopElse $ coreToAlgebra e3
                                  --Construct result
                                  let ks = keys ts2
                                  let cols = leafNames cs2
                                  let colsDiff = cols L.\\ ks
                                  n1 <- attach ordCol intT (int 1) q2
                                  q' <- rownum iterPrime ["iter", ordCol, "pos"] Nothing
                                            =<< union n1 
                                                =<< attach ordCol intT (int 2) q3
                                  let projPairs = zip colsDiff colsDiff ++ zip ks (repeat iterPrime)
                                  n2 <- proj (("iter","iter"):("pos","pos"):projPairs) q'
                                  ts <- mergeTableStructure q' ts2 ts3
                                  return (n2, cs2, ts)
-- Compile function application, as we do not have functions as results the given
-- argument can be evaluated and then be passed to the compileApp function.
coreToAlgebra (App _ e1 e2) = compileAppE1 e1 =<< compileParam e2
                                

-- | Transform the variable environment                                  
transformGam :: (AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)) 
                -> AlgNode -> Gam -> GraphM Gam
transformGam f loop gamma  = mapM (f loop) gamma

-- | Transformation of gamma for if then else    
algResLoop :: AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)
algResLoop loop (n, (i, cs, pl)) = do
                              i' <- eqJoin "iter" "iter" i loop
                              return (n, (i', cs, pl))

-- | Compile a function parameter
-- | Function is partial, i.e. it doesn't compile lambda's as arguments                              
compileParam :: Param -> GraphM AlgRes
compileParam (ParExpr _ e1) = coreToAlgebra e1
compileParam (ParAbstr _ _ _) = $impossible

-- | Compile function application.
-- | Expects a core expression the function, and the evaluated argument
compileAppE1 :: CoreExpr -> AlgRes -> GraphM AlgRes
compileAppE1 (App _ (Var _ "zip") (ParExpr _ e1)) (q2', cs2, (SubPlan ts2)) =
                do
                    (q1', cs1, (SubPlan ts1)) <- coreToAlgebra e1
                    q1 <- absPos q1' cs1
                    q2 <- absPos q2' cs2
                    let offSet = colSize cs1
                    let cs2' = incrCols offSet cs2
                    let projPairs1 = zip (leafNames cs1) (leafNames cs1)
                    let projPairs2 = zip (leafNames cs2') (leafNames cs2')
                    let projPairs2' = zip (leafNames cs2') (leafNames cs2) 
                    q <- eqTJoin [("iter", iterPrime), ("pos", posPrime)] (("iter", "iter"):("pos", "pos"):(projPairs1 ++ projPairs2)) q1
                            =<< proj ((iterPrime, "iter"):(posPrime, "pos"):projPairs2') q2
                    let cs = [NCol "1" cs1, NCol "2" cs2']
                    let ts = SubPlan $ M.union ts1 $ M.mapKeysMonotonic (+ offSet) ts2
                    return (q, cs, ts)
compileAppE1 (Var _ "unzip") (q, [NCol "1" cs1, NCol "2" cs2], (SubPlan ts)) =
               do
                   let (cs2d, d) = decrCols cs2
                   let projPairs1 = zip (leafNames cs1) (leafNames cs1)
                   let projPairs2 = zip (leafNames cs2d) (leafNames cs2)
                   q' <- proj [("iter", "iter"),("pos", "pos"), ("item1", "iter"), ("item2", "iter")]
                            =<< attach "pos" natT (nat 1) =<< getLoop
                   q1 <- proj (("iter", "iter"):("pos", "pos"):projPairs1) q
                   q2 <- proj (("iter", "iter"):("pos", "pos"):projPairs2) q
                   let cs = [NCol "1" [Col 1 surT], NCol "2" [Col 2 surT]]
                   let ln1 = leafNumbers cs1
                   let ln2 = leafNumbers cs2d
                   let ts1 = SubPlan $ M.fromList [(l, ts M.! l)  | l <- ln1, isJust $ M.lookup l ts]
                   let ts2 = SubPlan $ M.fromList [(l, ts M.! (l + d)) | l <- ln2, isJust $ M.lookup (l +d) ts]
                   let ts' = SubPlan $ M.fromList [(1, (q1, cs1, ts1)),(2, (q2, cs2d,ts2))]
                   return (q', cs, ts')
                    
compileAppE1 (App _ (Var _ "map") l@(ParAbstr _ _ _)) (q1, cs1, ts1) = 
                do
                    gam <- getGamma
                    (_qv', qv, mapv, loopv, gamV) <- mapForward gam q1 cs1
                    (q2, cs2, ts2) <- withContext gamV loopv $ compileLambda (qv, cs1, ts1) l
                    let csProj2 = zip (leafNames cs2) (leafNames cs2)
                    q <- proj (("iter",outer):("pos", posPrime):csProj2)
                            =<< eqJoin "iter" inner q2 mapv
                    return (q, cs2, ts2)
compileAppE1 (App _ (Var _ "max") (ParExpr _ e1)) (q2, [Col 1 t], _ts2) = 
                do
                    (q1, [Col 1 _], _ts1) <- coreToAlgebra e1
                    q <- proj [("iter", "iter"),("pos", "pos"),("item1", resCol)]    
                        =<< aggr [(Max, resCol, Just "item1")] Nothing 
                            =<< union q1 q2
                    return (q, [Col 1 t], emptyPlan)
compileAppE1 (App _ (Var _ "min") (ParExpr _ e1)) (q2, [Col 1 t], _ts2) =
                do
                    (q1, [Col 1 _], _ts1) <- coreToAlgebra e1
                    q <- proj [("iter", "iter"),("pos", "pos"),("item1", resCol)]    
                        =<< aggr [(Min, resCol, Just "item1")] Nothing 
                            =<< union q1 q2
                    return (q, [Col 1 t], emptyPlan)
compileAppE1 (App _ (Var _ "filter") l@(ParAbstr _ _ _)) (q1, cs1, ts1) =
                do
                    gam <- getGamma
                    (qv', qv, _mapv, loopv, gamV) <- mapForward gam q1 cs1
                    (q2, _cs2, _ts2) <- withContext gamV loopv $ compileLambda (qv, cs1, ts1) l
                    let csProj = zip (leafNames cs1) (leafNames cs1)
                    q <- proj (("iter", "iter"):("pos", "pos"):csProj)
                            =<< select resCol  
                                =<< eqJoin inner iterPrime qv' 
                                    =<< proj [(iterPrime, "iter"), (resCol, "item1")] q2
                    return (q, cs1, ts1)
compileAppE1 (Var _ "head") (q1', cs1, ts1) =
                do
                    q1 <- absPos q1' cs1
                    q <- posSelect 1 [("pos", Asc)] (Just "iter") q1
                    return (q, cs1, ts1)
compileAppE1 (Var _ "tail") (q1', cs1, ts1) =
                    do
                        let projPairs = zip (leafNames cs1) (leafNames cs1)
                        q1 <- absPos q1' cs1
                        q <- proj (("iter", "iter"):("pos", "pos"):projPairs)
                                =<< select resCol 
                                    =<< oper ">" resCol "pos" oldCol 
                                        =<< attach oldCol natT (nat 1) q1
                        return (q, cs1, ts1)
compileAppE1 (Var _ "concat") (q, _cs, SubPlan ts) =
                    do
                        let [(1, (qs, css, tss))] = M.toList ts
                        let projPairs = zip (leafNames css) (leafNames css)
                        q' <- proj (("iter", iterPrime):("pos", posPrimePrime):projPairs)
                                =<< rank posPrimePrime [(posPrime, Asc), ("pos", Asc)]
                                    =<< eqJoin "iter" resCol qs
                                        =<< proj [(iterPrime, "iter"),(posPrime, "pos"), (resCol, "item1")] q
                        return (q', css, tss)    
compileAppE1 (Var _ "nub") (q, cs, ts) =
                    do
                        let projPairs = ("iter", "iter"):("pos", "pos"):(zip (leafNames cs) (leafNames cs))
                        q' <- eqTJoin [("pos", posPrime), ("iter", iterPrime)]  projPairs q
                                =<< aggr [(Min, posPrime, Just "pos"), (Min, iterPrime, Just "iter")] (Just resCol)
                                    =<< rowrank resCol (map (\x -> (x, Asc)) ("iter":(leafNames cs))) q
                        return (q', cs, ts)
compileAppE1 (Var mt "count") (q, cs, ts) = compileAppE1 (Var mt "length") (q, cs, ts)
compileAppE1 (Var _ "length") (q, _cs, _ts) = 
                    do
                        loop <- getLoop
                        q''' <- aggr [(Count, "item1", Nothing)] (Just "iter") q
                        q'' <- attach "item1" intT (int 0)
                                =<< difference loop 
                                    =<< proj [("iter", "iter")] q'''
                        q' <- attach "pos" natT (nat 1)
                                =<< union q'' q''' 
                        return (q', [Col 1 AInt], emptyPlan)
                        
compileAppE1 (Var _ "box") (q, cs, ts) =
                    do
                        q' <- attach "pos" natT (nat 1) 
                                =<< proj [("iter", "iter"),("item1", "iter")] 
                                    =<< getLoop
                        return (q', [Col 1 surT], subPlan 1 (q, cs, ts))
compileAppE1 (Var mt@(_ :=> FFn _ t) "the") (q, cs, ts) = 
                                                     if (isPrim t) 
                                                      then
                                                       do 
                                                         let projPairs = (:) ("iter", "iter") $ zip (leafNames cs) (leafNames cs)
                                                         q' <- attach "pos" natT (nat 1) =<< distinct =<< proj projPairs q
                                                         return (q', cs, ts) 
                                                      else 
                                                        compileAppE1 (Var mt "head") (q, cs, ts)
compileAppE1 (Var mt "all") (q, cs, ts) = compileAppE1 (Var mt "and") (q, cs, ts)
compileAppE1 (Var _ "and") (q, cs, ts) =
                    do
                        q' <- attach "pos" natT (nat 1)
                                =<< proj [("iter", "iter"), ("item1", resCol)]
                                    =<< aggr [(Min, resCol, Just "item1")] (Just "iter")
                                        =<< union q
                                            =<< attach "pos" natT (nat 1) =<< attach "item1" boolT (bool True) =<< getLoop 
                        return (q', cs, ts)
compileAppE1 (Var (_ :=> FFn _ t) "sum") (q, cs, _) =
                    do
                        let ty = case t of
                                    FInt -> intT
                                    FFloat -> doubleT
                                    (FVar _) -> intT
                                    _ -> $impossible
                        loop <- getLoop
                        q' <- aggr [(Sum, "item1", Just "item1")] (Just "iter") q
                        q'' <- attach "item1" ty (int 0)
                                =<< difference loop 
                                    =<< proj [("iter", "iter")] q'
                        q''' <- attach "pos" natT (nat 1) 
                                    =<< union q'' q'
                        return (q''', cs, emptyPlan)
compileAppE1 (Var _ "or") (q, cs, ts) =
                    do
                        q' <- attach "pos" natT (nat 1)
                                =<< proj [("iter", "iter"), ("item1", resCol)]
                                    =<< aggr [(Max, resCol, Just "item1")] (Just "iter") 
                                        =<< union q
                                            =<< attach "pos" natT (nat 1) =<< attach "item1" boolT (bool False) =<< getLoop
                        return (q', cs, ts)
compileAppE1 (Var _ "not") (q, [Col 1 t], _ts) =
                    do
                        q' <- proj [("iter", "iter"), ("pos", "pos"), ("item1", resCol)]
                                =<< notC resCol "item1" q
                        return (q', [Col 1 t], emptyPlan)
compileAppE1 (Var _ "integerToDouble") (q, _cs, _ts) =
                    do
                        q' <- proj [("iter", "iter"), ("pos", "pos"), ("item1", resCol)]
                                =<< cast "item1" resCol ADouble q
                        return (q', [Col 1 ADouble], emptyPlan )
compileAppE1 (Var _ "unBox") (q, [Col 1 ASur], ts) = 
                    do
                        let (q', cs', ts') = getPlan 1 ts
                        let csProj = zip (leafNames cs') (leafNames cs')
                        q'' <- proj (("iter", iterPrime):("pos","pos"):csProj)
                                =<< eqJoin "iter" resCol q'
                                    =<< proj [(iterPrime, "iter"),(resCol, "item1")] q
                        return (q'', cs', ts')
compileAppE1 (App _ (App _ (Var _ "groupWith") e1@(ParAbstr _ _ _)) e2@(ParAbstr _ _ _)) (q3, cs3, ts3) =
            do
                gam <- getGamma
                (qv', qv, _map', loop', gam') <- mapForward gam q3 cs3
                (q1, cs1, ts1) <- withContext gam' loop' $ compileLambda (qv, cs3, ts3) e1
                (q2, cs2, _ts2) <- withContext gam' loop' $ compileLambda (qv, cs3, ts3) e2
                let offSet = colSize cs1
                let cs2' = incrCols offSet cs2
                let projPairs1 = zip (leafNames cs1) (leafNames cs1)
                let projPairs2 = zip (leafNames cs2') (leafNames cs2)
                q1' <- proj ((iterR, "iter"):projPairs1) q1
                q2' <- proj ((iterPrime, "iter"):projPairs2) q2
                qs <- eqJoin iterR iterPrime q1' q2'
                qvs <- proj [("iter", "iter"), ("pos", "pos"), (inner, inner)] qv'
                q <- rowrank resCol (map (\ki -> (ki, Asc)) ((:) "iter" $ leafNames cs2'))
                        =<< eqJoin inner iterPrime qvs qs
                let newCol = (+) 1 $ colSize cs2
                let projPairs2' = zip (leafNames cs2) (leafNames cs2')
                qout <- distinct =<< proj (("iter", "iter"):("pos", resCol):("item" ++ show newCol, resCol):projPairs2') q
                qin <- proj (("iter", resCol):("pos", "pos"):projPairs1) q
                let cs = [NCol "1" cs2, NCol "2" [Col newCol surT]]
                let ts = subPlan newCol (qin, cs1, ts1)
                return (qout, cs, ts)
compileAppE1 (App t2 (App t1 (Var mt "groupByN") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy'") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy1") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy2") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy3") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy4") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy5") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App t2 (App t1 (Var mt "groupBy6") e1) e2) e3 = compileAppE1 (App t2 (App t1 (Var mt "groupBy") e1) e2) e3
compileAppE1 (App _ (App _ (Var _ "groupBy") e1@(ParAbstr _ _ _)) e2@(ParAbstr _ _ _)) (q3, cs3, ts3) =
            do
                gam <- getGamma
                (qv', qv, _map', loop', gam') <- mapForward gam q3 cs3
                (q1, cs1, ts1) <- withContext gam' loop' $ compileLambda (qv, cs3, ts3) e1
                (q2, cs2, _ts2) <- withContext gam' loop' $ compileLambda (qv, cs3, ts3) e2
                let offSet = colSize cs1
                let cs2' = incrCols offSet cs2
                let projPairs1 = zip (leafNames cs1) (leafNames cs1)
                let projPairs2 = zip (leafNames cs2') (leafNames cs2)
                q1' <- proj ((iterR, "iter"):projPairs1) q1
                q2' <- proj ((iterPrime, "iter"):projPairs2) q2
                qs <- eqJoin iterR iterPrime q1' q2'
                qvs <- proj [("iter", "iter"), ("pos", "pos"), (inner, inner)] qv'
                q <- rowrank resCol (map (\ki -> (ki, Asc)) ((:) "iter" $ leafNames cs2'))
                        =<< eqJoin inner iterPrime qvs qs
                let nrFields = length cs1
                let projOut = zip ["item" ++ show i | i <- [1..nrFields]] $ repeat resCol
                qout <- distinct =<< proj (("iter", "iter"):("pos", resCol):projOut) q
                (ts, cs) <- makeSubPlan 1 cs1 ts1 q
                return (qout, cs, ts)
compileAppE1 e1 _ = error $ "Not implemented yet: " ++ show e1           


makeSubPlan :: Int -> Columns -> SubPlan -> AlgNode -> GraphM (SubPlan, Columns)
makeSubPlan 1 [Col _ t] (SubPlan ts) q = do
                                            qi <- proj [("iter", resCol),("pos", posPrime),("item1", "item1")] q
                                            let tsi = case M.lookup 1 ts of
                                                        Nothing -> emptyPlan
                                                        (Just p) -> subPlan 1 p
                                            return (subPlan 1 (qi, [Col 1 t], tsi), [Col 1 surT])
makeSubPlan i ((NCol n csi):css) (SubPlan ts) q = do
                                                    (SubPlan ts', cs') <- makeSubPlan (i + 1) css (SubPlan ts) q
                                                    let (csi', d) = decrCols csi
                                                    let ln = leafNumbers csi'
                                                    let projPairs = zip (leafNames csi') (leafNames csi)
                                                    qi <- proj (("iter", resCol):("pos", "pos"):projPairs) q
                                                    let tsi = SubPlan $ M.fromList [(l, ts M.! (l + d)) | l <- ln, isJust $ M.lookup (l + d) ts]
                                                    return (SubPlan $ M.insert i (qi, csi', tsi) ts', (NCol n [Col i surT]):cs')
                                                    
makeSubPlan _ [] _  _ = return (emptyPlan, [])
makeSubPlan _ _ _ _ = $impossible
                    
-- | Compile a lambda where the argument variable is bound to the given expression                    
compileLambda :: AlgRes -> Param -> GraphM AlgRes
compileLambda arg (ParAbstr _ (PVar x) e) = withBinding x arg $ coreToAlgebra e
compileLambda _ p = $impossible

-- | Transform gamma for map function                
algResv :: AlgNode -> (String, AlgRes) -> GraphM (String, AlgRes)
algResv m (n, (q, cs, ts)) = do
                                let projPairs = zip (leafNames cs) (leafNames cs)
                                q' <- proj (("iter", inner):("pos","pos"):projPairs) =<< eqJoin "iter" outer q m
                                return (n, (q', cs, ts))

keys :: SubPlan -> [String]
keys (SubPlan ts) = map (\i -> "item" ++ show i) $ M.keys ts 


mergeTableStructure :: AlgNode -> SubPlan -> SubPlan -> GraphM SubPlan
mergeTableStructure qo (SubPlan ts1') (SubPlan ts2') | M.null ts1' = return $ SubPlan ts2'
                                                     | M.null ts2' = return $ SubPlan ts1'
                                                     | otherwise = do
                                                        rs <- mapM mergeBinds items
                                                        return $ SubPlan $ M.fromList rs    
    where
        items = M.toList ts1'
        mergeBinds :: (Int, AlgRes) -> GraphM (Int, AlgRes)
        mergeBinds (i, (q1, cs1, ts1)) = do
                                            let (q2, _cs2, ts2) = case M.lookup i ts2' of
                                                                    Nothing -> error "jikes"
                                                                    Just a -> a
                                            let ks = keys ts1
                                            let cols = leafNames cs1
                                            let colsDiff = cols L.\\ ks
                                            let projPairs = zip cols cols
                                            let projPairsD = zip colsDiff colsDiff
                                            let projPairsKs = zip ks $ repeat iterPrime
                                            n1 <- attach ordCol intT (int 1) q1
                                            n2 <- attach ordCol intT (int 2) q2
                                            qo'' <- proj [(ordPrime, ordCol), (iterR, iterPrime), (oldCol, "item" ++ show i)] qo
                                            qo' <- eqTJoin [(ordPrime, ordCol), (oldCol, "iter")] 
                                                           (("iter", "iter"):(iterR, iterR):("pos", "pos"):(ordCol, ordCol):projPairs)
                                                           qo''
                                                           =<< union n1 n2
                                            q <- rownum iterPrime ["iter", ordCol, "pos"] Nothing qo'
                                            qr <- proj ((iterPrime, iterPrime):(ordCol, ordCol):projPairs) q
                                            q' <- proj (("iter", iterR):("pos", "pos"):(projPairsD ++ projPairsKs)) q
                                            ts' <- mergeTableStructure qr ts1 ts2
                                            return (i, (q', cs1, ts'))
                                            
mergeTableStructureFirst :: AlgNode -> SubPlan -> SubPlan -> GraphM SubPlan
mergeTableStructureFirst qo (SubPlan ts1') (SubPlan ts2') 
                            | M.null ts1' = return $ SubPlan ts2'
                            | M.null ts2' = return $ SubPlan ts1'
                            | otherwise= do
                                          rs <- mapM mergeBinds items
                                          return $ SubPlan $ M.fromList rs
     where
        items = M.toList ts1'
        mergeBinds :: (Int, AlgRes) -> GraphM (Int, AlgRes)
        mergeBinds (i, (q1, cs1, ts1)) = do 
                                            let (q2, _cs2, ts2) = ts2' M.! i
                                            let ks = keys ts1
                                            let cols = leafNames cs1
                                            let colsDiff = cols L.\\ ks
                                            let projPairs = zip cols cols
                                            let projPairsD = zip colsDiff colsDiff
                                            let projPairsKs = zip ks $ repeat iterPrime
                                            qo'' <- (proj [(ordPrime, ordCol),(iterR, iterPrime),(oldCol, "item" ++ show i)] qo)
                                            qo' <- eqTJoin [(ordPrime, ordCol), (oldCol, "iter")] 
                                                           (("iter", "iter"):(iterR,iterR):("pos", "pos"):(ordCol, ordCol):(iterPrime, iterPrime) : projPairs) 
                                                           qo''
                                                           =<< rownum iterPrime ["iter", ordCol, "pos"] Nothing
                                                            =<< flip union q2 =<< attach ordCol intT (int 1) q1
                                            qr <- proj ((iterPrime, iterPrime):(ordCol, ordCol):projPairs) qo'
                                            q' <- proj (("iter", iterR):("pos", "pos"):(projPairsD ++ projPairsKs)) qo'
                                            ts' <- mergeTableStructureFirst qr ts1 ts2
                                            return (i, (q', cs1, ts'))
                                            

mergeTableStructureLast :: Int -> SubPlan -> GraphM SubPlan
mergeTableStructureLast n (SubPlan ts1') = do
                                            rs <- mapM updateBinds items
                                            return $ SubPlan $ M.fromList rs
    where
        items = M.toList ts1'
        updateBinds :: (Int, AlgRes) -> GraphM (Int, AlgRes)
        updateBinds (i, (q1, cs1, ts1)) = do
                                            q <- attach ordCol intT (int $ toInteger n) q1
                                            ts <- mergeTableStructureLast n ts1
                                            return (i, (q, cs1, ts))
                                            
mergeTableStructureSeq :: Int -> SubPlan -> SubPlan -> GraphM SubPlan
mergeTableStructureSeq n (SubPlan ts1') (SubPlan ts2') 
                                | M.null ts1' = return $ SubPlan ts2'
                                | M.null ts2' = return $ SubPlan ts1'
                                | otherwise= do
                                              rs <- mapM mergeBinds items
                                              return $ SubPlan $ M.fromList rs
    where
        items = M.toList ts1'
        mergeBinds :: (Int, AlgRes) -> GraphM (Int, AlgRes)
        mergeBinds (i, (q1, cs1, ts1)) = do
                                            let (q2, _cs2, ts2) = ts2' M.! i
                                            q <- flip union q2 
                                                    =<< attach ordCol intT (int $ toInteger n) q1
                                            ts <- mergeTableStructureSeq n ts1 ts2
                                            return (i, (q, cs1, ts))

-- Compilation for the first element of a list.
-- For optimisation purposes we distinguish three cases:
-- Singleton lists: compile these if they were just single values
-- A list where the second element is also created through a list constructor
--      that particular case allows for optimising on the rank operator, it is
--      compiled to algebra in the listSequence function that does not perform rank.
-- A list where the tail is the result of a computation, the tail is compiled as a
--      normal expression. The result get an ord column attached and the is unified
--      with the head of the list and then ranked.    
listFirst :: CoreExpr -> GraphM AlgRes
listFirst (Cons _ e1 (Nil _)) = coreToAlgebra e1
listFirst (Cons _ e1 e2@(Cons _ _ _)) = do
                                         (q1, cs1, ts1) <- coreToAlgebra e1
                                         (q2, _cs2, ts2) <- listSequence e2 2
                                         let cols = leafNames cs1
                                         let ks = keys ts1
                                         let colsDiff = cols L.\\ ks
                                         let projPairs = (zip colsDiff colsDiff) ++ (zip ks $ repeat iterPrime) 
                                         q' <- rownum iterPrime ["iter", ordCol, "pos"] Nothing
                                                 =<< rank posPrime [(ordCol, Asc), ("pos", Asc)] 
                                                     =<< flip union q2 =<< attach ordCol intT (int 1) q1
                                         q <- proj (("iter", "iter"):("pos", posPrime) : projPairs) q'
                                         ts <- mergeTableStructureFirst q' ts1 ts2
                                         return (q, cs1, ts) 
listFirst (Cons _ e1 e2) = do
                            (q1, cs1, ts1) <- coreToAlgebra e1
                            (q2, _cs2, ts2) <- coreToAlgebra e2
                            let cols = leafNames cs1
                            let ks = keys ts1
                            let colsDiff = cols L.\\ ks
                            let projPairs = (zip colsDiff colsDiff) ++ (zip ks $ repeat iterPrime)
                            n1 <- attach ordCol intT (int 1) q1
                            q' <- rownum iterPrime ["iter", ordCol, "pos"] Nothing
                                    =<< rank posPrime [(ordCol, Asc), ("pos", Asc)]
                                        =<< union n1 
                                            =<< attach ordCol intT (int 2) q2
                            qr <- proj ((iterPrime, iterPrime):(ordCol, ordCol):(zip cols cols)) q'
                            q <- proj (("iter", "iter"):("pos", posPrime):projPairs) q'
                            ts <- mergeTableStructure qr ts1 ts2
                            return (q, cs1, ts)
listFirst _ = $impossible


-- List sequence, doesn't perform the rank operation, that is carried out by listFirst.
--  Three cases with similar motivation as listFirst.
listSequence :: CoreExpr -> Int -> GraphM AlgRes
listSequence (Cons _ e1 (Nil _)) n = do
                                      (q1, cs1, ts1) <- coreToAlgebra e1
                                      n1 <- attach ordCol intT (int $ toEnum n) q1
                                      ts <- mergeTableStructureLast n ts1
                                      return (n1, cs1, ts)
listSequence (Cons _ e1 e2@(Cons _ _ _)) n = do
                                                (q1, cs1, ts1) <- coreToAlgebra e1
                                                (q2, _cs2, ts2) <- listSequence e2 $ n + 1
                                                n1 <- attach ordCol intT (int $ toEnum n) q1
                                                n2 <- union n1 q2
                                                ts <- mergeTableStructureSeq n ts1 ts2
                                                return (n2, cs1, ts)
listSequence c@(Cons _ _ _) n = do
                                 (q, cs, ts) <- listFirst c
                                 n1 <- attach ordCol intT (int $ toEnum n) q
                                 ts' <- mergeTableStructureLast n ts
                                 return (n1, cs, ts')
listSequence _ _ = $impossible
                                    
-- Transform a record element into algebraic plan                             
recElemToAlgebra :: RecElem -> GraphM AlgRes
recElemToAlgebra (RecElem _ n e) = do
                                     (q1, cs1, ts1) <- coreToAlgebra e
                                     return (q1, [NCol n cs1], ts1)

-- Transform a record into an algebraic plan                                     
recElemsToAlgebra :: GraphM AlgRes -> RecElem -> GraphM AlgRes
recElemsToAlgebra alg2 el = do
                                (q1, cs1, (SubPlan ts1)) <- alg2
                                (q2, cs2, (SubPlan ts2)) <- recElemToAlgebra el
                                let offSet = colSize cs1
                                let cs2' = incrCols offSet cs2
                                let projPairs = zip (leafNames cs2') (leafNames cs2)
                                let ts = SubPlan $ M.union ts1 $ M.mapKeysMonotonic (+ offSet) ts2
                                n1 <- proj ((mkPrefixIter 1, "iter"):projPairs) q2
                                n2 <- eqJoin "iter" (mkPrefixIter 1) q1 n1
                                let projPairs' = zip (leafNames cs1) (leafNames cs1) ++ zip (leafNames cs2') (leafNames cs2')
                                n3 <- proj (("iter", "iter"):("pos", "pos"):projPairs') n2
                                return (n3, cs1 ++ cs2', ts)

-- map forward transforms the environment etc into the versions needed to compute in
-- a loop context. The result is (qv', qv, mapv, loopv, Gamv)
mapForward :: Gam -> AlgNode -> Columns -> GraphM (AlgNode, AlgNode, AlgNode, AlgNode, Gam)
mapForward gam q cs = do
                          let csProj = zip (leafNames cs) (leafNames cs)
                          qv' <- rownum inner ["iter", "pos"] Nothing q
                          qv  <- proj (("iter", inner):("pos", posPrime):csProj)
                                      =<< attach posPrime natT (nat 1) qv'
                          mapv <- proj [(outer, "iter"), (inner, inner), (posPrime, "pos")] qv'
                          loopv <- proj [("iter",inner)] qv'
                          gamV <- transformGam algResv mapv gam
                          return (qv', qv, mapv, loopv, gamV)
 
-- Recalculate the position column, making it densely populated after this operation
absPos :: AlgNode -> Columns -> GraphM AlgNode
absPos q cs = let projPairs = zip (leafNames cs) (leafNames cs)
               in proj (("iter", "iter"):("pos", "pos"):projPairs) 
                    =<< rownum "pos" [posPrime] (Just "iter")
                        =<< proj (("iter", "iter"):(posPrime, "pos"):projPairs) q
                         
-- Function to transform the column structure

--From a typedcore column list to algebraic columns
coreCol2AlgCol :: [T.Column] -> Columns
coreCol2AlgCol cols = map (\(Column s t, i) -> NCol s $ fst $ typeToCols t i) cols'
    where
      cols' = zip cols [1..]

--Translate core keys to algebraic keys
key2Key :: Columns -> [Key] -> KeyInfos
key2Key cs ks = map (\(Key k) -> map (\ki -> case getCol ki cs of
                                                [(Col i _)] -> "item" ++ show i
                                                [] -> $impossible
                                                (NCol _ _) : _ -> $impossible
                                                (Col _ _) : (_ : _) -> $impossible) k ) ks

-- Get all the column names from the structure                                    
leafNames :: Columns -> [String]
leafNames cs = map (\c -> case c of
                            (Col i _) -> "item" ++ show i
                            _         -> error "Named column not allowed in leafNames") $ colLeafs cs

leafNumbers :: Columns -> [Int]
leafNumbers cs = map (\c -> case c of
                            (Col i _) -> i
                            _         -> error "Named column not allowed in LeafNumbers") $ colLeafs cs

-- Get all the leaf columns, that is the columns that are actually a column
colLeafs :: Columns -> Columns
colLeafs (c@(Col _ _):xs) = (:) c $ colLeafs xs
colLeafs ((NCol _ cs):xs) = colLeafs cs ++ colLeafs xs
colLeafs []               = []

-- Count the number of columns
colSize :: Columns -> Int
colSize = length . colLeafs

-- Increment the column numbers by a given amount
incrCols :: Int -> Columns -> Columns
incrCols inc ((Col i t):xs)    = (Col (i + inc) t):(incrCols inc xs)
incrCols inc ((NCol x i):xs) = (NCol x (incrCols inc i)):(incrCols inc xs)
incrCols _   []              = [] 

-- Find the lowest column number
minCol :: Columns -> Int
minCol c = minimum $ map (\c' -> case c' of
                                    (Col i _) -> i
                                    _         -> error "Named column not expected in minCol") $ colLeafs c

-- Decrement the column numbers so that the lowest column number is 1 after applying
decrCols :: Columns -> (Columns, Int)
decrCols cols = let minV = minCol cols
                 in (decr' (minV - 1) cols, minV  - 1)
    where
     decr' :: Int -> Columns -> Columns
     decr' decr ((Col i t):xs)    = (flip Col t $ i - decr) : (decr' decr xs)
     decr' decr ((NCol x i):xs) = (NCol x $ decr' decr i) : (decr' decr xs)
     decr' _    []              = []

-- Find the columns associated with a record label
getCol :: String -> Columns -> Columns
getCol n cs = getCol' cs
    where
     getCol' :: Columns -> Columns
     getCol' ((Col _ _):xs)              = getCol' xs
     getCol' ((NCol x i):xs) | x == n    = i
                             | otherwise = getCol' xs
     getCol' []                          = $impossible -- error $ show n ++ " in " ++ show cs --[]

-- Transform Columns info into schema info for algebraic compilation
colsToSchema :: Columns -> SchemaInfos
colsToSchema ((Col i t):xs) = (:)("item" ++ show i, t) $ colsToSchema xs
colsToSchema ((NCol _ cs):xs) = colsToSchema cs ++ colsToSchema xs
colsToSchema [] = []

-- Transform a type to columns structure
typeToCols :: FType -> Int -> (Columns, Int)
typeToCols (FRec recs) i = recsToCols recs i
typeToCols FInt i = ([Col i AInt], i + 1)
typeToCols FBool i = ([Col i ABool], i + 1)
typeToCols FFloat i = ([Col i ADouble], i + 1)
typeToCols FString i = ([Col i AStr], i + 1)
typeToCols FUnit i = ([Col i AInt], i + 1)
typeToCols (FList _) i = ([Col i ASur], i + 1)
typeToCols (FVar _) i = ([Col i ANat], i + 1)
typeToCols _ _ = $impossible

-- Compile a record type to a column structure
recsToCols :: [(RLabel, FType)] -> Int -> (Columns, Int)
recsToCols ((RLabel s, ty):xs) i = let (cs, i') = typeToCols ty i
                                       (cs', i'') = recsToCols xs i'
                                    in ((NCol s cs):cs',  i'')
recsToCols [] i = ([], i)
recsToCols ((RGen _, _) : _) _ = $impossible
recsToCols ((RVar _, _) : _) _ = $impossible