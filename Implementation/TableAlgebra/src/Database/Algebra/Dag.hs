module Database.Algebra.Dag
       (
         -- * The DAG data structure
         AlgebraDag
       , Operator
       , nodeMap
       , rootNodes
       , mkDag
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Graph.Inductive.PatriciaTree
  
import Database.Algebra.Graph.Common
{-

general:

want to keep tidying up to a minimum (garbage collection)
all operations should only consider nodes that are valid, i.e. reachable from roots

relevant operations: topsort!, parents, reachableNodesFrom
  
cache reachable nodes -> DagRewrite
-}

data AlgebraDag a = AlgebraDag { nodeMap       :: NodeMap a   -- ^ Return the nodemap of a DAG
                               , graph         :: UGr         -- ^ Auxilliary representation for topological information
                               , rootNodes     :: [AlgNode]   -- ^ Return the (possibly modified) list of root nodes from a DAG
                               }

class Operator a where
    opChildren :: a -> [AlgNode]
    replaceOpChild :: a -> AlgNode -> AlgNode -> a
    
-- | Create a DAG from a map of NodeIDs and algebra operators and a list of root nodes.
mkDag :: Operator a => NodeMap a -> [AlgNode] -> AlgebraDag a
mkDag m rs = AlgebraDag { nodeMap = m, graph = g, rootNodes = rs }
    where g = uncurry G.mkUGraph $ M.foldrWithKey aux ([], []) m
          aux n op (allNodes, allEdges) = (n : allNodes, es ++ allEdges)
              where es = map (\v -> (n, v)) $ opChildren op
