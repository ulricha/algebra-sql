module Database.Algebra.Graph.Common where

import qualified Data.Map as M

-- FIXME this should be abstract
type AlgNode = Int 

type AlgMap alg = M.Map alg AlgNode
type NodeMap a = M.Map AlgNode a

type Tags = M.Map AlgNode [String]

