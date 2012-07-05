{-# LANGUAGE DeriveGeneric #-}

module Database.Algebra.VL.Render.JSON(serializePlan, deserializePlan, planToFile, planFromFile) where

import GHC.Generics(Generic)
import qualified Data.Map as M
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (ToJSON, FromJSON, encode, decode)

import Database.Algebra.Dag.Common
import Database.Algebra.VL.Data

instance ToJSON TerOp where
instance ToJSON BinOp where
instance ToJSON UnOp where
instance ToJSON NullOp where
instance ToJSON VLVal where
instance ToJSON VLType where
instance ToJSON VecOp where
instance (ToJSON t, ToJSON b, ToJSON u, ToJSON n, ToJSON c) => ToJSON (Algebra t b u n c) where
    
instance FromJSON TerOp where
instance FromJSON BinOp where
instance FromJSON UnOp where
instance FromJSON NullOp where
instance FromJSON VLVal where
instance FromJSON VLType where
instance FromJSON VecOp where
instance (FromJSON t, FromJSON b, FromJSON u, FromJSON n, FromJSON c) => FromJSON (Algebra t b u n c) where

instance ToJSON Plan where
instance FromJSON Plan where
   
data Plan = Plan {tags :: [(AlgNode, [Tag])], roots :: [AlgNode], graph :: [(AlgNode, VL)]}
    deriving Generic
             
serializePlan :: (NodeMap [Tag], [AlgNode], NodeMap VL) -> BL.ByteString
serializePlan (ts, rs, g) = let tags' = M.toList ts
                                graph' = M.toList g
                             in encode $ Plan {tags = tags', roots = rs, graph = graph'}

deserializePlan :: BL.ByteString -> (NodeMap [Tag], [AlgNode], NodeMap VL)
deserializePlan s = let Just (Plan ts rs g) = decode s
                    in (M.fromList ts, rs, M.fromList g)
    
planToFile :: FilePath -> (NodeMap [Tag], [AlgNode], NodeMap VL) -> IO ()
planToFile f t = BL.writeFile f $ serializePlan t

planFromFile :: FilePath -> IO (NodeMap [Tag], [AlgNode], NodeMap VL)
planFromFile f = liftM deserializePlan $ BL.readFile f