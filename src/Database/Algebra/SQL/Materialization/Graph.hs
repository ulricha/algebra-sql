-- | Provides required graph tools.
module Database.Algebra.SQL.Materialization.Graph
    ( Graph
    , Vertex
    , mkGraph
    , parents
    , children
    , node
    , topSort
    , vertices
    ) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.Graph.Inductive.Query.DFS as D

-- | The vertex type.
type Vertex = G.Node

-- | The graph type.
newtype Graph label = Graph
                    { graph :: P.Gr label ()
                    }

-- | Constructs a graph from the given out-adjacency list.
mkGraph :: [(label, Vertex, [Vertex])] -> Graph label
mkGraph outAdjacencyList = Graph $ G.mkGraph ns es
  where ns            = map nf outAdjacencyList
        nf (n, k, _)  = (k, n)
        ef (_, k, ks) = map (tf k) ks
        es            = concatMap ef outAdjacencyList
        tf a b        = (a, b, ())

-- | Fetches the parents of a vertex.
parents :: Vertex -> Graph label -> [Vertex]
parents v g = G.pre (graph g) v

-- | Fetches the children of a vertex.
children :: Vertex -> Graph label -> [Vertex]
children v g = G.suc (graph g) v

-- | Fetches the label of a vertex.
node :: Vertex -> Graph label -> Maybe label
node v g = G.lab (graph g) v

-- | Sorts the vertices topological.
topSort :: Graph label -> [Vertex]
topSort = D.topsort . graph

-- | Gets all vertices from a given graph.
vertices :: Graph label -> [Vertex]
vertices = G.nodes . graph

