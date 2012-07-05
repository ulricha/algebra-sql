module Database.Algebra.VL.Render.JSON where

import Database.Algebra.Dag.Common
  
import Database.Algebra.VL.Data

import qualified Data.Map as M

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (ToJSON, encode)

instance ToJSON TerOp where
instance ToJSON BinOp where
instance ToJSON UnOp where
instance ToJSON NullOp where
instance ToJSON VLVal where
instance ToJSON VLType where
instance ToJSON VecOp where
instance (ToJSON t, ToJSON b, ToJSON u, ToJSON n, ToJSON c) => ToJSON (Algebra t b u n c) where
    
renderVLJSON :: NodeMap [Tag] -> [AlgNode] -> NodeMap VL -> String
renderVLJSON ts rootNodes nodeMap = BL.unpack $ encode (M.toList ts, rootNodes, M.toList nodeMap)



    
    