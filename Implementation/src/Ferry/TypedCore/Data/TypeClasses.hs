module Ferry.TypedCore.Data.TypeClasses where
    
import Ferry.TypedCore.Data.Base
import Ferry.TypedCore.Data.Type
import Ferry.Compiler.Error.Error
import Control.Monad

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

-- Type Class stuff (based on M. P. Jones Typing Haskell in Haskell)

type Class = ([Ident], [Inst])
type Inst  = Qual Pred

type ClassEnv = M.Map Ident Class

type ClassEnvTransformer = ClassEnv -> Either FerryError ClassEnv

infixr 5 <:>
(<:>) :: ClassEnvTransformer -> ClassEnvTransformer -> ClassEnvTransformer
(f <:> g) ce = do
                ce' <- f ce
                g ce'
                
defined :: Ident -> ClassEnv -> Bool
defined = M.member

addClass :: Ident -> [Ident] -> ClassEnvTransformer
addClass c sc ce
    | defined c ce    = Left $ ClassAlreadyDefinedError c
    | any (not . flip defined ce) sc = Left $ SuperClassNotDefined c sc
    | otherwise = Right $ M.insert c (sc, []) ce
    
addInstance :: [Pred] -> Pred -> ClassEnvTransformer
addInstance ps p@(IsIn i _) ce | not (defined i ce) = Left $ ClassNotDefined i
                               | otherwise          = Right $ M.insert i (c, inst:is) ce 
                                 where 
                                  (c, is) = getClass i ce
                                  inst = ps :=> p 
                                      
getClass :: Ident -> ClassEnv -> Class
getClass i m = m M.! i

emptyClassEnv :: ClassEnv
emptyClassEnv = M.empty