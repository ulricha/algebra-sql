{-# LANGUAGE DeriveGeneric #-}
module Database.Algebra.Dag.Common where

import qualified Data.Map as M
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

-- | Identifiers for DAG nodes.
type AlgNode = Int 

type AlgMap alg = M.Map alg AlgNode
type NodeMap a = M.Map AlgNode a

type Tag = String

-- | Tertiary, Binary, unary and leaf nodes of a relational algebra DAG.
data Algebra t b u n c = TerOp t c c c
                       | BinOp b c c
                       | UnOp u c
                       | NullaryOp n
                         deriving (Ord, Eq, Show, Read, Generic)

instance (ToJSON t, ToJSON b, ToJSON u, ToJSON n, ToJSON c) => ToJSON (Algebra t b u n c) where
instance (FromJSON t, FromJSON b, FromJSON u, FromJSON n, FromJSON c) => FromJSON (Algebra t b u n c) where