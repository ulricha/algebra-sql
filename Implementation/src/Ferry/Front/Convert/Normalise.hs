{-# LANGUAGE TypeSynonymInstances#-}
module Ferry.Front.Convert.Normalise (runNormalisation) where

import Ferry.Front.Data.Language
import Ferry.Front.Data.Base
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Instances
import Ferry.Compiler.Error.Error

import Control.Monad.State
import Control.Monad.Error
import Control.Applicative (Applicative(..), (<**>), (<$>), (<*>))

import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Maybe (fromJust)


-- Substitutions are stored in a map, a variable name is always substituted by a new one
type Substitution = M.Map Identifier Expr

-- The transformation monad
-- The outcome is a pair of the outcome or an error and a state.
-- The state contains substitutions and a supply of fresh variables
type Normalisation = ErrorT FerryError (State (Int, Substitution))

instance Applicative Normalisation where
    (<*>) = ap
    pure = return
    
restoreState :: Normalisation a -> Normalisation a
restoreState e = do
                    (_, s) <- get
                    e' <- e
                    (c, _) <- get
                    put (c, s)
                    return e'
    
-- Convenience function for operations on the monad

-- Add a substitution
addSubstitution :: Identifier -> Expr -> Normalisation ()
addSubstitution i n = do 
                        modify (\(v, m) ->  (v, M.insert i n m))

-- Apply a substitution to the given identifier
applySubstitution :: Expr -> Normalisation Expr
applySubstitution v@(Var _ i) = do
                        (_,m) <- get
                        case M.lookup i m of
                            Nothing -> return v
                            Just i' -> normalise i'

-- Remove the list of given identifiers from the substitution list
removeSubstitution :: [Identifier] -> Normalisation ()
removeSubstitution is = do
                         modify (\(v, m) -> (v, foldr M.delete m is))

-- Retrieve a fresh variable name from the monad, the state is updated accordingly                         
getFreshIdentifier :: Normalisation String
getFreshIdentifier = do
                        (n, subst) <- get
                        put (n + 1, subst)
                        return $ "_v" ++ show n

-- Retrieve a fresh variable from the monad                        
getFreshVariable :: Normalisation Expr
getFreshVariable = do
                    n <- getFreshIdentifier
                    return $ Var (Meta emptyPos) n

-- Transform the given expression
runNormalisation :: Expr -> Either FerryError Expr
runNormalisation x = fst $ flip runState (0, M.empty) $ runErrorT $ (normalise x)

-- Instances of the normalise class

normaliseArg :: Arg -> Normalisation Arg
normaliseArg (AExpr m e) = do
                         e' <- normalise e
                         return $ AExpr m e'
normaliseArg (AAbstr m p e) = restoreState $
                            do
                             let vss = concatMap vars p
                             let vs = map vars p
                             ns <- normIdent p
                             let newSubsts = concat $ map makeSubstitution ns
                             removeSubstitution vss
                             mapM (\(i, ex) -> addSubstitution i ex) newSubsts
                             e' <- normalise e
                             let p' = replacePattern p $ map snd ns
                             return $ AAbstr m [p'] e'
--FIX
normIdent :: [Pattern] -> Normalisation [(Pattern, Identifier)]
normIdent [] = return []
normIdent (x:xs) = (\y ys -> (x, y):ys) <$> getFreshIdentifier <*> normIdent xs

replacePattern :: [Pattern] -> [Identifier] -> Pattern
replacePattern [p] [v] = PVar emptyMeta v
replacePattern ps vs = PPat emptyMeta vs     
                            
makeSubstitution :: (Pattern, Identifier) -> [(Identifier, Expr)]
makeSubstitution ((PVar m i), f) = [(i, Var m f)]
makeSubstitution ((PPat m vs), f) = [(v, Elem emptyMeta (Var emptyMeta f) $ Right i) | (v, i) <- zip vs [1..]]

normaliseRec :: RecElem -> Normalisation RecElem
normaliseRec r@(TrueRec m s e) = 
                            case (s, e) of
                             (Right i, Just ex) -> (\e' -> TrueRec m s $ Just e') <$> normalise ex
                             (Right i, Nothing) -> pure $ TrueRec m s $ Just (Var m i)
                             (Left i, Nothing) -> case i of
                                                    (Elem m e (Left x)) -> (\i' -> TrueRec m (Right x) $ Just i') <$> normalise i
                                                    (_)                 ->  throwError $ IllegalRecSyntax r
                             (_) -> throwError $ IllegalRecSyntax r
normaliseRec (TuplRec m i e) = TuplRec m i <$> normalise e

normaliseBinding :: Binding -> Normalisation Binding
normaliseBinding (Binding m s e) = do
                                    e' <- normalise e
                                    removeSubstitution [s]
                                    return $ Binding m s e'

normalise :: Expr -> Normalisation Expr
normalise c@(Const _ _) = return c
normalise (UnOp m o e) = UnOp m o <$> normalise e
normalise (BinOp m o e1 e2) = BinOp m o <$> normalise e1 <*> normalise e2
normalise v@(Var m i) = applySubstitution v
normalise (App m e a) = do 
                         v <- getFreshIdentifier 
                         case e of
                          (Var m "lookup") -> normalise $ Elem m ex $ Right 2                                                 
                           where
                               ex = App m (Var m "single") [AExpr m filterApp]
                               filterApp = App m (Var m "filter") [AAbstr m [(PVar m v)] comp, AExpr m el]
                               comp = BinOp m (Op m "==") (Elem m (Var m v) $ Right 1) ek
                               [e1, e2] = a 
                               el = case e2 of
                                     AExpr _ e' -> e'
                               ek = case e1 of
                                     AExpr _ e' -> e'
                          _ -> do
                                e' <- restoreState $ normalise e
                                as <- mapM normaliseArg a
                                return $ App m e' as 
normalise (If m e1 e2 e3) =  If m <$> normalise e1 <*> normalise e2 <*> normalise e3
normalise (Record  m els) = case (head els) of
                             (TrueRec _ _ _) ->
                                (\e -> Record m $ L.sortBy sortElem e) <$>  mapM normaliseRec els
                             (TuplRec _ _ _) -> Record m <$> mapM normaliseRec els
normalise (Paren _ e) = normalise e
normalise (List m es) = List m <$> mapM normalise es
normalise (Elem m e i) = do 
                           e' <- normalise e
                           (_, s) <- get
                           seq (putStrLn $ "hello " ++ show s) $ return $ Elem m e' i -- <$> normalise e <*> return i
normalise (Lookup m e1 e2) = normalise $ App m (Var m "lookup") [AExpr (getMeta e2) e2, AExpr (getMeta e1) e1]
normalise (Let m bs e) = restoreState $ 
                          case bs of
                            [b] -> (\b' n -> Let m [b'] n) <$> normaliseBinding b <*> normalise e
                            (b:bss) -> (\b' tl -> Let m [b'] tl) <$> normaliseBinding b <*> (normalise $ Let m bss e)
normalise (Table m s cs ks) = pure $ Table m s (L.sortBy sortColumn cs) ks
normalise (Relationship m c1 e1 c2 e2 k1 k2) = do
                v1 <- getFreshIdentifier
                v2 <- getFreshIdentifier
                cg <- compgen v1 v2 k1 k2
                let filterF = AAbstr m [(PVar m v2)] cg
                let filterApp = App m (Var m "filter") [filterF, AExpr m e2]
                case (c1, c2) of
                    (One _, One _) -> do                                            
                                        let single = App m (Var m "single") [AExpr m filterApp]  
                                        normalise $ App m (Var m "map") [ AAbstr m [(PVar m v1)] 
                                                                                   (Record m [ TuplRec m 1 (Var m v1)
                                                                                             , TuplRec m 2 single
                                                                                             ])
                                                                        , AExpr m e1]
                    (One _, Many _) -> do
                                         normalise $ App m (Var m "map") [ AAbstr m [(PVar m v1)] 
                                                                                    (Record m [ TuplRec m 1 (Var m v1)
                                                                                              , TuplRec m 2 filterApp
                                                                                              ])
                                                                         , AExpr m e1]
normalise (QComp m q) = normaliseQCompr q
    
compgen :: String -> String -> Key -> Key -> Normalisation Expr
compgen v1 v2 k1@(Key m1 ks1) k2@(Key m2 ks2) = if (length ks1 == length ks2)
                                                 then pure $ foldl1 ands $ zipWith equal v1s v2s
                                                 else throwError $ IncompatableKeys k1 k2
    where
        ands = (\e1 e2 -> BinOp m1 (Op m1 "and") e1 e2)
        equal = (\e1 e2 -> BinOp m1 (Op m1 "==") e1 e2)
        rec1 = (\f -> Elem m1 (Var m1 v1) $ Left f)
        v1s = [rec1 k | k <- ks1]
        rec2 = (\f -> Elem m2 (Var m2 v2) $ Left f)
        v2s = [rec2 k | k <- ks2]
        
sortColumn :: Column -> Column -> Ordering
sortColumn (Column _ s1 _) (Column _ s2 _) = compare s1 s2

sortElem :: RecElem -> RecElem -> Ordering
sortElem (TrueRec _ (Right i1) _) (TrueRec _ (Right i2) _) = compare i1 i2
sortElem _                        _                        = error "illegal input to sortElem Normalise.hs"


normaliseQCompr :: QCompr -> Normalisation Expr
normaliseQCompr (FerryCompr m bs bd r) = 
    restoreState $ case bs of
                    [bs'] ->
                     case bd of
                         [] -> normaliseReturn m bs' r
                         (For _ b):bd' -> normaliseQCompr $ FerryCompr m (bs':b) bd' r
                         (ForWhere _ e):bd' -> normaliseWhere m e bs' bd' r
                         (ForLet m' b):(ForLet _ b'):bd' -> normaliseQCompr $ FerryCompr m [bs'] ((ForLet m' $ b ++ b'):bd') r
                         (ForLet _ b):bd' -> normaliseLet m bs' b bd' r
                         (ForOrder _ os):bd' -> normaliseOrder m bs' os bd' r
                         b@(Group m' g me es mp):bd' -> case length es of
                                                           1 -> normaliseGroup m bs' b bd' r
                                                           _ -> normaliseQCompr $ FerryCompr m [bs'] (gb:bd') r
                                                            where
                                                               gb = Group m' g me [(Record m' [TuplRec m' i e | (e, i) <- (zip es [1..])])] mp
                    bs    -> do
                              let b1 = head bs
                              let b2 = head $ tail bs
                              let bt = tail $ tail bs
                              bs' <- normaliseCompr b1 b2
                              r' <- case r of
                                      (Return m' e mi) -> do 
                                                            e' <- normalise e
                                                            return $ Return m' e' mi 
                              normaliseQCompr $ FerryCompr m (bs':bt) bd r'
                                                                                
normaliseQCompr (HaskellCompr _ _ _) = error "Not implemented HaskellCompr"

normaliseReturn :: Meta -> (Pattern, Expr) -> ReturnElem -> Normalisation Expr
normaliseReturn m (p1,e1) (Return m' e2 mi) = case mi of
                                             Nothing -> normalise $ App m (Var emptyMeta "map") [AAbstr emptyMeta [p1] e2, AExpr emptyMeta e1]
                                             Just (p2, bd, r) -> let eIn = QComp m $ FerryCompr m [(p1,e1)] [] (Return m' e2 Nothing)  
                                                                  in normaliseQCompr $ FerryCompr m' [(p2, eIn)] bd r 

normaliseGroup :: Meta -> (Pattern, Expr) -> BodyElem -> [BodyElem] -> ReturnElem -> Normalisation Expr
normaliseGroup m (p, e) (Group _ g me [eg] mi) bd r = 
    do
      let f = case g of {GBy -> "groupBy"; GWith -> "groupWith"} 
      star <- getFreshIdentifier
      let arg1 = case me of
                    Nothing -> AAbstr emptyMeta [PVar emptyMeta star] $ Var emptyMeta star
                    Just ev -> AAbstr emptyMeta [p] ev
      let eb = App emptyMeta (Var emptyMeta f) [arg1, AAbstr emptyMeta [p] eg
                                                    , AExpr emptyMeta e]
      let pr = case mi of {Nothing -> p; (Just p2) -> p2}
      normaliseQCompr $ FerryCompr m [(pr, eb)] bd r


normaliseOrder :: Meta -> (Pattern, Expr) -> [ExprOrder] -> [BodyElem] -> ReturnElem -> Normalisation Expr
normaliseOrder m (p, e) ords bd r = (\(AExpr _ app) -> normaliseQCompr $ FerryCompr m [(p, app)] bd r) 
                                     $ foldr f (AExpr emptyMeta e) $ (ordToFn1 $ head ords):(map ordToFn $ tail ords)
  where    
    f :: (Expr, Expr) -> Arg -> Arg
    f (fn, e) a = AExpr emptyMeta (App emptyMeta fn [AAbstr emptyMeta [p] e, a]) 
    ordToFn (ExprOrder m e o) = (Var m $ case o of
                                          Ascending _ -> "thenBy"
                                          Descending _ -> "thenByDescending", e) 
    ordToFn1 (ExprOrder m e o) = (Var m $ case o of
                                           Ascending _ -> "orderBy"
                                           Descending _ -> "orderByDescending", e)
    
normaliseLet :: Meta -> (Pattern, Expr) -> [(Pattern, Expr)] -> [BodyElem] -> ReturnElem -> Normalisation Expr
normaliseLet m (p, e) letB bd r = do
                                    star <- getFreshIdentifier
                                    mapM (\(i, ex) -> addSubstitution i ex) (substs star)
                                    normaliseQCompr $ FerryCompr m [(PVar emptyMeta star, app)] bd r
  where 
    app = App emptyMeta (Var emptyMeta "map") [arg1, arg2]
    arg1 = AAbstr emptyMeta [p] flatRec
    arg2 = AExpr emptyMeta e
    substs s = [(i, e') | (i, e') <- [(v, Elem emptyMeta (Var emptyMeta s) $ Left v) | v <- vars p ++ (map fst $ splitBinds letB)]]
    flatRec = Record emptyMeta $ [TrueRec emptyMeta (Right n) (Just $ Var emptyMeta n) | n <- vars p] 
                                    ++ [TrueRec emptyMeta (Right n) (Just e') | (n, e') <- splitBinds letB]
    splitBinds ((PVar _ v, e'):bs) = (v, e'):(splitBinds bs)
    splitBinds []                  = []
    
    
normaliseWhere :: Meta -> Expr -> (Pattern, Expr) -> [BodyElem] -> ReturnElem -> Normalisation Expr
normaliseWhere m f (p1, e1) bd r = normaliseQCompr $ FerryCompr m [(p1, e1')] bd r
  where
    e1' = App emptyMeta (Var emptyMeta "filter") [AAbstr emptyMeta [p1] f, AExpr emptyMeta e1] 

normaliseCompr :: (Pattern, Expr) -> (Pattern, Expr) -> Normalisation (Pattern, Expr)
normaliseCompr (p1, e1) (p2, e2) = do
                                 star <- getFreshIdentifier
                                 let newSubst = [(v, Elem emptyMeta (Var emptyMeta star) $ Left v) | v <- vars p1 ++ vars p2 ]
                                 mapM (\(i, ex) -> addSubstitution i ex) newSubst
                                 e1' <- normalise e1
                                 e2' <- normalise e2
                                 pure $ (PVar m star, rExpr e1' e2')
    where m = Meta emptyPos
          v1s = case p1 of
                 PVar _ s -> [s]
                 PPat _ vs -> vs
          v2s = case p2 of
                 PVar _ s -> [s]
                 PPat _ vs -> vs
          rExpr e1 e2 = App m (Var m "concatMap" ) [arg1 e2, arg2, arg3 e1]
          arg1 e2 = AAbstr m [p1] e2
          arg2 = AAbstr m [p1, p2] $ Record m $ [TrueRec m (Right n) (Just $ Var m n) | n <- vars p1] 
                                               ++ [TrueRec m (Right n) (Just $ Var m n) | n <- vars p2]
          arg3 e1 = AExpr (getMeta e1) e1
                    


