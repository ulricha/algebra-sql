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
instance ToJSON Nat where
instance ToJSON VLVal where
instance ToJSON VLType where
instance ToJSON VecOp where
instance ToJSON DescrProj where
instance ToJSON PosProj where
instance ToJSON PayloadProj where
instance ToJSON ISTransProj where
instance ToJSON Expr2 where
instance ToJSON Expr1 where
instance ToJSON LeftCol where
instance ToJSON RightCol where

instance FromJSON TerOp where
instance FromJSON BinOp where
instance FromJSON UnOp where
instance FromJSON NullOp where
instance FromJSON Nat where
instance FromJSON VLVal where
instance FromJSON VLType where
instance FromJSON VecOp where
instance FromJSON DescrProj where
instance FromJSON PosProj where
instance FromJSON PayloadProj where
instance FromJSON ISTransProj where
instance FromJSON Expr2 where
instance FromJSON Expr1 where
instance FromJSON LeftCol where
instance FromJSON RightCol where

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