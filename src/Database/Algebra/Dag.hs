module Database.Algebra.Dag
       (
         -- * The DAG data structure
         AlgebraDag
       , Operator(..)
       , nodeMap
       -- FIXME refCountMap is only exposed for debugging -> remove
       , refCountMap
       , rootNodes
       , mkDag
         -- * Query functions for topological and operator information
       , parents
         -- FIXME is topological sorting still necessary?
       , topsort
       , hasPath
       , reachableNodesFrom
       , operator
         -- * DAG modification functions
       , insert
       , delete
       , replace
       , replaceChild
       , replaceRoot
       ) where

import           Control.Exception.Base
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import qualified Data.IntMap                       as M
import qualified Data.List                         as L
import qualified Data.Set                          as S
import           Text.Printf

import           Database.Algebra.Dag.Common

import           Debug.Trace

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
                               , refCountMap   :: NodeMap Int -- ^ A map storing the number of parents for each nod e.
                               }

class Ord a => Operator a where
    opChildren :: a -> [AlgNode]
    replaceOpChild :: a -> AlgNode -> AlgNode -> a

-- For every node, count the number of parents (or list of edges to the node).
-- We don't consider the graph a multi-graph, so an edge (u, v) is only counted
-- once.  We insert one virtual edge for every root node, to make sure that root
-- nodes are not pruned if they don't have any incoming edges.
initRefCount :: Operator o => [AlgNode] -> NodeMap o -> NodeMap Int
initRefCount rs nm = L.foldl' incParents (M.foldr' insertEdge M.empty nm) rs
  where insertEdge op rm = L.foldl' incParents rm (L.nub $ opChildren op)
        incParents rm n  = M.insert n ((M.findWithDefault 0 n rm) + 1) rm

-- | Create a DAG from a map of NodeIDs and algebra operators and a list of root nodes.
mkDag :: Operator a => NodeMap a -> [AlgNode] -> AlgebraDag a
mkDag m rs = AlgebraDag { nodeMap = mNormalized
                        , graph = g
                        , rootNodes = rs
                        , refCountMap = initRefCount rs mNormalized
                        }
    where mNormalized = normalizeMap rs m
          g =  uncurry G.mkUGraph $ M.foldrWithKey aux ([], []) mNormalized
          aux n op (allNodes, allEdges) = (n : allNodes, es ++ allEdges)
              where es = map (\v -> (n, v)) $ opChildren op

reachable :: Operator a => NodeMap a -> [AlgNode] -> S.Set AlgNode
reachable m rs = L.foldl' traverse S.empty rs
  where traverse :: S.Set AlgNode -> AlgNode -> S.Set AlgNode
        traverse s n = L.foldl' traverse (S.insert n s) (opChildren $ lookupOp n)

        lookupOp n = case M.lookup n m of
                       Just op -> op
                       Nothing -> error $ "node not present in map: " ++ (show n)

normalizeMap :: Operator a => [AlgNode] -> NodeMap a -> NodeMap a
normalizeMap rs m =
  let reachableNodes = reachable m rs
  in M.filterWithKey (\n _ -> S.member n reachableNodes) m

-- Utility functions to maintain the reference counter map and eliminate no
-- longer referenced nodes.

lookupRefCount :: AlgNode -> AlgebraDag a -> Int
lookupRefCount n d =
  case M.lookup n (refCountMap d) of
    Just c  -> c
    Nothing -> error $ "no refcount value for node " ++ (show n)

decrRefCount :: AlgebraDag a -> AlgNode -> AlgebraDag a
decrRefCount d n =
  let refCount = lookupRefCount n d
      refCount' = assert (refCount /= 0) $ refCount - 1
  in d { refCountMap = M.insert n refCount' (refCountMap d) }

-- | Delete a node from the node map and the aux graph
-- Beware: this leaves the DAG in an inconsistent state, because
-- reference counters have to be updated.
delete' :: Operator a => AlgNode -> AlgebraDag a -> AlgebraDag a
delete' n d =
    let g' = G.delNode n $ graph d
        m' = M.delete n $ nodeMap d
    in d { nodeMap = m', graph = g' }

-- Cut an edge to a node reference counting wise.
-- If the ref count becomes zero, the node is deleted and the children are
-- traversed.
cutEdge :: Operator a => AlgebraDag a -> AlgNode -> AlgebraDag a
cutEdge d edgeTarget =
  trace ("cutting edge to " ++ (show edgeTarget)) $
  let d'          = decrRefCount d edgeTarget
      newRefCount = lookupRefCount edgeTarget d'
  in if newRefCount == 0
     then let cs = opChildren $ operator edgeTarget d'
              d'' = trace ("deleting " ++ (show edgeTarget)) $ delete' edgeTarget d'
          in L.foldl' cutEdge d'' cs
     else d'


-- Don't decrement the ref counter of the top target node edgeTarget.
-- Prune it if it's already zero and then traverse the children.
-- This version is used for replace operations, where we might replace
-- an edge with the same edge.
cutEdge' :: Operator a => AlgebraDag a -> AlgNode -> AlgebraDag a
cutEdge' d edgeTarget =
  trace ("cutting edge (nondecr) to " ++ (show edgeTarget)) $
  if (lookupRefCount edgeTarget d) == 0
  then let cs = opChildren $ operator edgeTarget d
           d' = trace ("deleting " ++ (show edgeTarget)) $ delete' edgeTarget d
       in L.foldl' cutEdge d' cs
  else d

addEdgeTo :: AlgebraDag a -> AlgNode -> AlgebraDag a
addEdgeTo d n =
  let refCount = lookupRefCount n d
  in assert (refCount /= 0) $ d { refCountMap = M.insert n (refCount + 1) (refCountMap d) }

-- | Replace an entry in the list of root nodes with a new node. The root node must be
-- present in the DAG.
replaceRoot :: Operator a => AlgebraDag a -> AlgNode -> AlgNode -> AlgebraDag a
replaceRoot d old new =
  if old `elem` (rootNodes d)
  then trace (printf "replaceRoot %d -> %d" old new) $
       let rs'         = map doReplace $ rootNodes d
           doReplace r = if r == old then new else r
           d'          = trace ((show $ rootNodes d) ++ (show rs')) $ d { rootNodes = rs' }
       in -- cut the virtual edge to the old root
          -- and insert a virtual edge to the new root
          assert (old /= new) $ (flip addEdgeTo new $ cutEdge d' old)
  else d

-- | Insert a new node into the DAG.
insert :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
insert n op d =
    let cs  = opChildren op
        g'  = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
        m'  = M.insert n op $ nodeMap d
    in L.foldl' addEdgeTo (d { nodeMap = m', graph = g' }) cs

-- | Delete a node from the DAG
-- Beware: This combinator should be avoided, as it might leave the DAG in
-- an inconsistent state when edges to n are not cared for.
-- FIXME: remove combinator
delete :: Operator a => AlgNode -> AlgebraDag a -> AlgebraDag a
delete n d =
    let cs = opChildren $ operator n d
        g' = G.delNode n $ graph d
        m' = M.delete n $ nodeMap d
        d' = d { nodeMap = m', graph = g' }
    in L.foldl' cutEdge d' cs

-- Update reference counters if nodes are not simply inserted or deleted but
-- edges are replaced by other edges.  We must not delete nodes before the new
-- edges have been taken into account. Only after that can we certainly say that
-- a node is no longer referenced (edges might be replaced by themselves).
replaceEdgesRef :: Operator a => [AlgNode] -> [AlgNode] -> AlgebraDag a -> AlgebraDag a
replaceEdgesRef oldChildren newChildren d =
  trace (printf "replaceEdgesRef %s %s" (show oldChildren) (show newChildren)) $
  let -- First, decrement refcounters for the old children
      d'  = L.foldl' decrRefCount d oldChildren
      -- Then, increment refcounters for the new children
      d'' = L.foldl' addEdgeTo d' newChildren
  -- We are now sure that nodes with refcounter = 0 are not referenced
  -- by new edges.
  in L.foldl' cutEdge' d'' oldChildren

replace :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
replace node newOp d =
  let oldChildren = opChildren $ operator node d
      newChildren = opChildren newOp
      nm'         = M.insert node newOp $ nodeMap d
      g'          = G.delEdges [ (node, c) | c <- oldChildren ] $ graph d
      g''         = G.insEdges [ (node, c, ()) | c <- newChildren ] g'
      d'          = d { nodeMap = nm', graph = g'' }
  in replaceEdgesRef oldChildren newChildren d'

-- | Return the list of parents of a node.
parents :: AlgNode -> AlgebraDag a -> [AlgNode]
parents n d = G.pre (graph d) n

-- | 'replaceChild n old new' replaces all links from node n to node old with links to node new.
replaceChild :: Operator a => AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild n old new d =
  trace (printf "replaceChild %d: %d -> %d" n old new) $
  let op = operator n d
  in if old `elem` opChildren op
     then let m' = M.insert n (replaceOpChild op old new) $ nodeMap d
              g' = G.insEdge (n, new, ()) $ G.delEdge (n, old) $ graph d
              d' = d { nodeMap = m', graph = g' }
          in replaceEdgesRef [old] [new] d'
     else d

-- | Returns the operator for a node.
operator :: AlgNode -> AlgebraDag a -> a
operator n d =
    case M.lookup n $ nodeMap d of
        Just op -> op
        Nothing -> error $ "AlgebraDag.operator: lookup failed for " ++ (show n)

-- | Return a topological ordering of all nodes which are reachable from the root nodes.
topsort :: Operator a => AlgebraDag a -> [AlgNode]
topsort d = DFS.topsort $ graph d

-- | Return all nodes that are reachable from one node.
reachableNodesFrom :: AlgNode -> AlgebraDag a -> S.Set AlgNode
reachableNodesFrom n d = S.fromList $ DFS.reachable n $ graph d

-- | Tests wether there is a path from the first to the second node.
hasPath :: AlgNode -> AlgNode -> AlgebraDag a -> Bool
hasPath a b d = b `S.member` (reachableNodesFrom a d)

