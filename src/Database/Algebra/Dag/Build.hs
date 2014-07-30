{-# LANGUAGE GADTs #-}

module Database.Algebra.Dag.Build where

import           Control.Monad.State
import qualified Data.IntMap                 as IM
import qualified Data.Map                    as M

import           Database.Algebra.Aux
import           Database.Algebra.Dag.Common


data BuildState alg = BuildState 
    { supply :: Int           -- ^ Supply for fresh node IDs
    , algMap :: AlgMap alg    -- ^ A map from nodes to their IDs
    , tags :: NodeMap [Tag]   -- ^ Tags for nodes
    }

-- | The DAG builder monad, abstracted over the algebra stored in the
-- DAG. Internally, the monad detects sharing of subgraphs via hash
-- consing.
type Build alg = State (BuildState alg)

-- | Variable environemtn mapping from variables to compiled nodes.
type Gam a = [(String, a)]

-- | An algebraic plan is the result of constructing a graph.
-- | The pair consists of the mapping from nodes to their respective ids
-- | and the algres from the top node.
type AlgPlan alg a = (AlgMap alg, a, NodeMap [Tag])

-- | Evaluate the monadic graph into an algebraic plan, given a loop
-- relation.
runBuild :: Build alg a -> AlgPlan alg a
runBuild m =  constructAlgPlan $ runState m initialBuildState
  where initialBuildState = BuildState { supply = 1, algMap = M.empty, tags = IM.empty }
        constructAlgPlan (r, s) = (algMap s, r, tags s)
        
reverseAlgMap :: AlgMap alg -> NodeMap alg
reverseAlgMap = reverseToIntMap

-- | Tag a subtree with a comment
tag :: String -> AlgNode -> Build alg AlgNode
tag s c = do
  addTag c s
  return c

-- | Tag a subtree with a comment (monadic version)
tagM :: String -> Build alg AlgNode -> Build alg AlgNode
tagM s = (=<<) (tag s)

-- Add tag
addTag :: AlgNode -> String -> Build alg ()
addTag i c = modify insertTag
  where
    insertTag :: BuildState a -> BuildState a
    insertTag s = s { tags = IM.insertWith (++) i [c] $ tags s }

{-
-- | Get the current variable environment
getGamma :: Build alg (Gam res)
getGamma = do
            (g, _) <- ask
            return g
-}

-- | Get a fresh node id
getFreshId :: Build alg Int
getFreshId = do
                s <- get
                let n = supply s
                put $ s { supply = n + 1 }
                return n

-- | Check if a node already exists in the graph construction environment, if so return its id.
findNode :: Ord alg => alg -> Build alg (Maybe AlgNode)
findNode n = do
              m <- gets algMap
              return $ M.lookup n m

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.
insertNode :: Ord alg => alg -> Build alg AlgNode
insertNode n = do
                            v <- findNode n
                            case v of
                                (Just n') -> return n'
                                Nothing -> insertNode' n

-- | Blindly insert a node, get a fresh id and return that
insertNode' :: Ord alg => alg  -> Build alg AlgNode
insertNode' n = do
                              i <- getFreshId
                              s <- get
                              let m' = M.insert n i (algMap s)
                              put $ s { algMap = m' }
                              return i

{-
-- | Evaluate the graph construction computation with the current environment extended with a binding n to v.
withBinding :: String -> a -> Build a alg r -> Build a alg r
withBinding n v a = local (\(g, alg) -> ((n, v):g, alg)) a

-- | Evaluate the graph construction computation with a differnt
-- gamma, and loop table. Return within the current computational
-- context.
withContext :: Gam a -> AlgNode -> Build a alg r -> Build a alg r
withContext gam loop = local (\_ -> (gam, loop))

-- | Lookup a variable in the environment
fromGam :: String -> Build a alg a
fromGam n = do
             (m, _) <- ask
             case lookup n m of
                 Just r -> return r
                 Nothing -> error $ "Variable: " ++ n ++ " could not be found, should not be possible!"

-}
