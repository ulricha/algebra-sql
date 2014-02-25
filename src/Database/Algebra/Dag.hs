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
       , insertNoShare
       , replaceChild
       , replaceRoot
       , collect
       ) where

import           Control.Exception.Base
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import qualified Data.IntMap                       as IM
import qualified Data.List                         as L
import qualified Data.Map                          as M
import qualified Data.Set                          as S

import           Database.Algebra.Dag.Common

data AlgebraDag a = AlgebraDag
  { nodeMap     :: NodeMap a       -- ^ Return the nodemap of a DAG
  , opMap       :: M.Map a AlgNode -- ^ reverse index from operators to nodeids
  , nextNodeID  :: AlgNode         -- ^ the next node id to be used when inserting a node
  , graph       :: UGr             -- ^ Auxilliary representation for topological information
  , rootNodes   :: [AlgNode]       -- ^ Return the (possibly modified) list of root nodes from a DAG
  , refCountMap :: NodeMap Int     -- ^ A map storing the number of parents for each nod e.
  }

class (Ord a, Show a) => Operator a where
    opChildren :: a -> [AlgNode]
    replaceOpChild :: a -> AlgNode -> AlgNode -> a

-- For every node, count the number of parents (or list of edges to the node).
-- We don't consider the graph a multi-graph, so an edge (u, v) is only counted
-- once.  We insert one virtual edge for every root node, to make sure that root
-- nodes are not pruned if they don't have any incoming edges.
initRefCount :: Operator o => [AlgNode] -> NodeMap o -> NodeMap Int
initRefCount rs nm = L.foldl' incParents (IM.foldr' insertEdge IM.empty nm) rs
  where insertEdge op rm = L.foldl' incParents rm (L.nub $ opChildren op)
        incParents rm n  = IM.insert n ((IM.findWithDefault 0 n rm) + 1) rm

initOpMap :: Ord o => NodeMap o -> M.Map o AlgNode
initOpMap nm = IM.foldrWithKey (\n o om -> M.insert o n om) M.empty nm

-- | Create a DAG from a map of NodeIDs and algebra operators and a list of root nodes.
mkDag :: Operator a => NodeMap a -> [AlgNode] -> AlgebraDag a
mkDag m rs = AlgebraDag { nodeMap = mNormalized
                        , graph = g
                        , rootNodes = rs
                        , refCountMap = initRefCount rs mNormalized
                        , opMap = initOpMap m
                        , nextNodeID = 1 + (fst $ IM.findMax m)
                        }
    where mNormalized = normalizeMap rs m
          g =  uncurry G.mkUGraph $ IM.foldrWithKey aux ([], []) mNormalized
          aux n op (allNodes, allEdges) = (n : allNodes, es ++ allEdges)
              where es = map (\v -> (n, v)) $ opChildren op

reachable :: Operator a => NodeMap a -> [AlgNode] -> S.Set AlgNode
reachable m rs = L.foldl' traverse S.empty rs
  where traverse :: S.Set AlgNode -> AlgNode -> S.Set AlgNode
        traverse s n = if S.member n s
                       then s
                       else L.foldl' traverse (S.insert n s) (opChildren $ lookupOp n)

        lookupOp n = case IM.lookup n m of
                       Just op -> op
                       Nothing -> error $ "node not present in map: " ++ (show n)

normalizeMap :: Operator a => [AlgNode] -> NodeMap a -> NodeMap a
normalizeMap rs m =
  let reachableNodes = reachable m rs
  in IM.filterWithKey (\n _ -> S.member n reachableNodes) m

-- Utility functions to maintain the reference counter map and eliminate no
-- longer referenced nodes.

lookupRefCount :: AlgNode -> AlgebraDag a -> Int
lookupRefCount n d =
  case IM.lookup n (refCountMap d) of
    Just c  -> c
    Nothing -> error $ "no refcount value for node " ++ (show n)

decrRefCount :: AlgebraDag a -> AlgNode -> AlgebraDag a
decrRefCount d n =
  let refCount = lookupRefCount n d
      refCount' = assert (refCount /= 0) $ refCount - 1
  in d { refCountMap = IM.insert n refCount' (refCountMap d) }

-- | Delete a node from the node map and the aux graph
-- Beware: this leaves the DAG in an inconsistent state, because
-- reference counters have to be updated.
delete' :: Operator a => AlgNode -> AlgebraDag a -> AlgebraDag a
delete' n d =
  let op     = operator n d
      g'     = G.delNode n $ graph d
      m'     = IM.delete n $ nodeMap d
      rc'    = IM.delete n $ refCountMap d
      opMap' = case M.lookup op $ opMap d of
                 Just n' | n == n' -> M.delete op $ opMap d
                 _                 -> opMap d
  in d { nodeMap = m', graph = g', refCountMap = rc', opMap = opMap' }

refCountSafe :: AlgNode -> AlgebraDag o -> Maybe Int
refCountSafe n d = IM.lookup n $ refCountMap d

collect :: Operator o => S.Set AlgNode -> AlgebraDag o -> AlgebraDag o
collect collectNodes d = S.foldl' tryCollectNode d collectNodes
  where tryCollectNode :: (Show o, Operator o) => AlgebraDag o -> AlgNode -> AlgebraDag o
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
  let d'          = decrRefCount d edgeTarget
      newRefCount = lookupRefCount edgeTarget d'
  in if newRefCount == 0
     then let cs  = opChildren $ operator edgeTarget d'
              d'' = delete' edgeTarget d'
          in L.foldl' cutEdge d'' cs
     else d'

addRefTo :: AlgebraDag a -> AlgNode -> AlgebraDag a
addRefTo d n =
  let refCount = lookupRefCount n d
  in assert (refCount /= 0) $ d { refCountMap = IM.insert n (refCount + 1) (refCountMap d) }

-- | Replace an entry in the list of root nodes with a new node. The root node must be
-- present in the DAG.
replaceRoot :: Operator a => AlgebraDag a -> AlgNode -> AlgNode -> AlgebraDag a
replaceRoot d old new =
  if old `elem` (rootNodes d)
  then let rs'         = map doReplace $ rootNodes d
           doReplace r = if r == old then new else r
           d'          = d { rootNodes = rs' }
       in -- cut the virtual edge to the old root
          -- and insert a virtual edge to the new root
          assert (old /= new) $ addRefTo (decrRefCount d' old) new
  else d

-- | Insert a new node into the DAG.
insert :: Operator a => a -> AlgebraDag a -> (AlgNode, AlgebraDag a)
insert op d =
  -- check if an equivalent operator is already present
  case M.lookup op $ opMap d of
    Just n  -> (n, d)
    Nothing ->
      -- no operator can be reused, insert a new one
      let cs     = opChildren op
          n      = nextNodeID d
          g'     = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
          m'     = IM.insert n op $ nodeMap d
          rc'    = IM.insert n 0 $ refCountMap d
          opMap' = M.insert op n $ opMap d
          d'     = d { nodeMap = m'
                     , graph = g'
                     , refCountMap = rc'
                     , opMap = opMap'
                     , nextNodeID = n + 1
                     }
      in (n, L.foldl' addRefTo d' cs)

insertNoShare :: Operator a => a -> AlgebraDag a -> (AlgNode, AlgebraDag a)
insertNoShare op d =
  -- check if an equivalent operator is already present
  let cs     = opChildren op
      n      = nextNodeID d
      g'     = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
      m'     = IM.insert n op $ nodeMap d
      rc'    = IM.insert n 0 $ refCountMap d
      opMap' = M.insert op n $ opMap d
      d'     = d { nodeMap = m'
                 , graph = g'
                 , refCountMap = rc'
                 , opMap = opMap'
                 , nextNodeID = n + 1
                 }
  in (n, L.foldl' addRefTo d' cs)

-- Update reference counters if nodes are not simply inserted or deleted but
-- edges are replaced by other edges.  We must not delete nodes before the new
-- edges have been taken into account. Only after that can we certainly say that
-- a node is no longer referenced (edges might be replaced by themselves).
replaceEdgesRef :: Operator a => [AlgNode] -> [AlgNode] -> AlgebraDag a -> AlgebraDag a
replaceEdgesRef oldChildren newChildren d =
  let -- First, decrement refcounters for the old children
      d'  = L.foldl' decrRefCount d oldChildren
      -- Then, increment refcounters for the new children
  in L.foldl' addRefTo d' newChildren

-- | Return the list of parents of a node.
parents :: AlgNode -> AlgebraDag a -> [AlgNode]
parents n d = G.pre (graph d) n

-- | 'replaceChild n old new' replaces all links from node n to node old with links to node new.
replaceChild :: Operator a => AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild n old new d =
  let op = operator n d
  in if old `elem` opChildren op && old /= new
     then let m' = IM.insert n (replaceOpChild op old new) $ nodeMap d
              g' = G.insEdge (n, new, ()) $ G.delEdge (n, old) $ graph d
              d' = d { nodeMap = m', graph = g' }
          in replaceEdgesRef [old] [new] d'
     else d

-- | Returns the operator for a node.
operator :: Operator a => AlgNode -> AlgebraDag a -> a
operator n d =
    case IM.lookup n $ nodeMap d of
        Just op -> op
        Nothing -> error $ "AlgebraDag.operator: lookup failed for " ++ (show n) ++ "\n" ++ (show $ map fst $ IM.toList $ nodeMap d)

-- | Return a topological ordering of all nodes which are reachable from the root nodes.
topsort :: Operator a => AlgebraDag a -> [AlgNode]
topsort d = DFS.topsort $ graph d

-- | Return all nodes that are reachable from one node.
reachableNodesFrom :: AlgNode -> AlgebraDag a -> S.Set AlgNode
reachableNodesFrom n d = S.fromList $ DFS.reachable n $ graph d

-- | Tests wether there is a path from the first to the second node.
hasPath :: AlgNode -> AlgNode -> AlgebraDag a -> Bool
hasPath a b d = b `S.member` (reachableNodesFrom a d)
