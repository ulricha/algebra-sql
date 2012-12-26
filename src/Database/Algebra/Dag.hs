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
       , replaceChild
       , replaceRoot
       , collect
       ) where

{-

Necessary API:

-> query
rootNodes (?)
parents
topsort (?)
hasPath (?)
reachableNodesFrom (?)
operator

-> modification
insert
replace' (not necessary as a primitive, can be implemented in DagRewrite)
replaceChild
replaceRoot (?)

-}

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

Problem: the similarity to garbage collection does not really hold: We must not
only not delete a node if it is still referenced in the DAG (DAG-level edge),
but also, if a reference to the node is still held by the user, that is a
rewrite rule still holds a reference to a node.

Possible solutions:

1. model this somehow, that is register references to a node explicitly.

Idea: register all nodes which you want to relink to or from explicitly.

Problems: awful code propably, plus: explicit references would have to be
explicitly dereferenced propably.

2. Explicitly trigger pruning of nodes: have a 'commit' action which triggers
rewriting and is necessary to get the DAG into a consistent state.

Problem: In a single rule, the graph is not necessarily consistent. Can this
become a problem?

Inside of a rule: Problematic is a parent lookup which returns nodes which are
no longer reachable. In that case, we haven't inferred properties for this node,
so that the lookup fails.

However, during one rule, properties are not reinferred. A node which is made
unreachable by a rewrite action will still have its properties around since we
are still in the same rule. After the rule, it will be pruned by 'commit', so
that it will no longer occur anyway.

Implementation: for every rewrite action which unlinks nodes (e.g. replace
etc.), record the node that is replaced (as part of the state -> DagRewrite).
Then: pass a list of nodes to commit

  commit :: [AlgNode] -> AlgebraDag o -> AlgebraDag o

and start pruning from these node.

Problem again: the parts of the graph that are to be pruned according to the
given top nodes might overlap: Pruning the first node might remove nodes which
would be removed by the second node as well. This is propably not a problem, we
just have to be careful to accept failing node lookups during pruning.

The commit call could be inserted in Template Haskell automatically after the
statement sequence to eliminate a possibility for errors.

3. Order all rewrite actions in a way that nodes are only unlinked when they are
no longer referenced. This is error prone and will fail. Not an option.

Plan of action:

0. re-design the rewriting API -> replaceRoot, replace etc.

1. In all rewrite actions, remove pruning and maintenance of the reference
counter.

2. Add a commit combinator to DAG.

3. Add further state to DagRewrite: list of nodes which were unlinked

4. maintain this list in DagRewrite actions.

5. Insert a call to commit in the TH code.

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
    let g'  = G.delNode n $ graph d
        m'  = M.delete n $ nodeMap d
        rc' = M.delete n $ refCountMap d
    in d { nodeMap = m', graph = g', refCountMap = rc' }


refCountSafe :: AlgNode -> AlgebraDag o -> Maybe Int
refCountSafe n d = M.lookup n $ refCountMap d

{-
TODO
For each node in the list (fold):
1. check if its unreferenced -> refcount == 0?
2. if yes: delete it, cut links to children, collect recursively
-}

collect :: Operator o => S.Set AlgNode -> AlgebraDag o -> AlgebraDag o
collect collectNodes d = S.foldl' tryCollectNode d collectNodes
  where tryCollectNode :: Operator o => AlgebraDag o -> AlgNode -> AlgebraDag o
        tryCollectNode di n =
          case refCountSafe n di of
            Just rc -> if rc == 0
                       then -- node is unreferenced -> collect it
                            let cs = opChildren $ operator n di
                                d' = delete' n di
                            in L.foldl' cutEdge d' cs

                       else di -- node is still referenced
            Nothing -> di

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

addRefTo :: AlgebraDag a -> AlgNode -> AlgebraDag a
addRefTo d n =
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
          assert (old /= new) $ addRefTo (decrRefCount d' old) new
  else d

-- | Insert a new node into the DAG.
insert :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
insert n op d =
    let cs  = opChildren op
        g'  = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
        m'  = M.insert n op $ nodeMap d
    in L.foldl' addRefTo (d { nodeMap = m', graph = g' }) cs

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
  in L.foldl' addRefTo d' newChildren

-- | Return the list of parents of a node.
parents :: AlgNode -> AlgebraDag a -> [AlgNode]
parents n d = G.pre (graph d) n

-- | 'replaceChild n old new' replaces all links from node n to node old with links to node new.
-- This is the internal variant which does not perform garbage collection.
replaceChild :: Operator a => AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild n old new d =
  trace (printf "replaceChild %d: %d -> %d" n old new) $
  let op = operator n d
  in if old `elem` opChildren op && old /= new
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

{-
{-
-}
replaceChild' :: AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild' = undefined

{-
1. look up parents
2. for each parent:
-}
replace :: AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replace = undefined

{-
-}
replaceChild :: AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild = undefined

-}

{-
replaceOp :: AlgNode -> o -> AlgebraDag o -> AlgebraDag o
replaceOp = undefined
-replace :: (Operator a, Show a) => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
-replace node newOp d =
-  trace (printf "replace %d -> %s" node (show newOp)) $
-  let oldChildren = opChildren $ operator node d
-      newChildren = opChildren newOp
-      nm'         = M.insert node newOp $ nodeMap d
-      g'          = G.delEdges [ (node, c) | c <- oldChildren ] $ graph d
-      g''         = G.insEdges [ (node, c, ()) | c <- newChildren ] g'
-      d'          = d { nodeMap = nm', graph = g'' }
-  in replaceEdgesRef oldChildren newChildren d'
+  in L.foldl' addRefTo d' newChildren
-}
