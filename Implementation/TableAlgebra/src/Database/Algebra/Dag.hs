module Database.Algebra.Dag
       (
         -- * The DAG data structure
         AlgebraDag
       , Operator
       , nodeMap
       , rootNodes
       , mkDag
         -- * Query functions for topological information
       , parents
       , topsort
       , hasPath
       , reachableNodesFrom
       , operator
         -- * DAG modification functions
       , insert
       , delete
       , replaceChild 
         -- * House cleaning
       , pruneUnused
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
          
-- | Replace an entry in the list of root nodes with a new node. The root node must be
-- present in the DAG.
replaceRoot :: AlgebraDag a -> AlgNode -> AlgNode -> AlgebraDag a
replaceRoot d old new = d { rootNodes = rs' }
  where rs' = map doReplace $ rootNodes d
        doReplace r = if r == old then new else old
        
-- | Insert a new node into the DAG.
insert :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
insert n op d = 
    let cs = opChildren op
        g' = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
        m' = M.insert n op $ nodeMap d
    in d { nodeMap = m', graph = g' }
       
-- | Delete a node from the DAG.
delete :: Operator a => AlgNode -> AlgebraDag a -> AlgebraDag a
delete n d =
    let g' = G.delNode n $ graph d
        m' = M.delete n $ nodeMap d
    in d { nodeMap = m', graph = g' }
       
-- | Return the list of parents of a node.
parents :: AlgNode -> AlgebraDag a -> [AlgNode]
parents n d = G.pre (graph d) n
              
-- | 'replaceChild n old new' replaces all links from node n to node old with links to node new.
replaceChild :: Operator a => AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild n old new d = 
    let m' = M.insert n (replaceOpChild (operator n d) old new) $ nodeMap d
        g' = G.insEdge (n, new, ()) $ G.delEdge (n, old) $ graph d
    in d { nodeMap = m', graph = g' }
       
-- | Returns the operator for a node.
operator :: AlgNode -> AlgebraDag a -> a
operator n d = 
    case M.lookup n $ nodeMap d of
        Just op -> op
        Nothing -> error $ "AlgebraDag.operator: lookup failed for " ++ (show n)
    
-- | Return a topological ordering of all nodes which are reachable from the root nodes.
-- The second parameter is the set of nodes which are reachable from the root nodes.
topsort :: Operator a => AlgebraDag a -> [AlgNode]
topsort d = filter (flip S.member (reachableNodes d)) $ DFS.topsort $ graph d
            
-- | Return the set of nodes that are reachable from at least one root node
reachableNodes :: AlgebraDag a -> S.Set AlgNode
reachableNodes d = S.fromList $ concatMap (flip DFS.reachable $ graph d) $ rootNodes d
                   
-- | Prune unreferenced nodes (i.e. unreachable from any root node) from the DAG.
pruneUnused :: AlgebraDag a -> Maybe (AlgebraDag a)
pruneUnused d =
    let g = graph d
        m = nodeMap d 
        roots = rootNodes d
        allNodes = S.fromList $ G.nodes g
        reachable = reachableNodes d
        unreachable = S.difference allNodes reachable
    in if S.null unreachable
       then Nothing
       else let g' = G.delNodes (S.toList $ unreachable) g
                m' = S.fold M.delete m unreachable
            in Just $ d { nodeMap = m', graph = g' }
    
-- | Return all nodes that are reachable from one node.
reachableNodesFrom :: AlgNode -> AlgebraDag a -> S.Set AlgNode
reachableNodesFrom n d = S.fromList $ DFS.reachable n $ graph d
                     
-- | Tests wether there is a path from the first to the second node.
hasPath :: AlgNode -> AlgNode -> AlgebraDag a -> Bool
hasPath a b d = b `S.member` (reachableNodesFrom a d)
