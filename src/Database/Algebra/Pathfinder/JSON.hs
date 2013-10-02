--JSON
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module Database.Algebra.Pathfinder.JSON(serializePlan, deserializePlan, planToFile, planFromFile) where

import GHC.Generics (Generic)    
import Control.Monad

import Database.Algebra.Dag.Common
import Database.Algebra.Pathfinder.Data.Algebra 
import qualified Data.IntMap as M
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (FromJSON, ToJSON, decode, encode)


instance ToJSON Column where
instance ToJSON ATy where
instance ToJSON AVal where
instance ToJSON SortDir where
instance ToJSON JoinRel where
instance ToJSON Fun where
instance ToJSON SortInf where
instance ToJSON Fun1to1 where
instance ToJSON RelFun where
instance ToJSON AggrType where
instance ToJSON NullOp where
instance ToJSON UnOp where
instance ToJSON BinOp where

instance FromJSON Column where
instance FromJSON ATy where
instance FromJSON AVal where
instance FromJSON SortDir where
instance FromJSON JoinRel where
instance FromJSON Fun where
instance FromJSON SortInf where
instance FromJSON Fun1to1 where
instance FromJSON RelFun where
instance FromJSON AggrType where
instance FromJSON NullOp where
instance FromJSON UnOp where
instance FromJSON BinOp where

instance ToJSON Plan where
instance FromJSON Plan where
   
data Plan = Plan { tags :: [(AlgNode, [Tag])], roots :: [AlgNode], graph :: [(AlgNode, PFAlgebra)] }
    deriving Generic

serializePlan :: (NodeMap [Tag], [AlgNode], NodeMap PFAlgebra) -> BL.ByteString
serializePlan (ts, rs, g) = let tags' = M.toList ts
                                graph' = M.toList g
                             in encode $ Plan {tags = tags', roots = rs, graph = graph'}

deserializePlan :: BL.ByteString -> (NodeMap [Tag], [AlgNode], NodeMap PFAlgebra)
deserializePlan s = let Just (Plan ts rs g) = decode s
                     in (M.fromList ts, rs, M.fromList g)

planToFile :: FilePath -> (NodeMap [Tag], [AlgNode], NodeMap PFAlgebra) -> IO ()
planToFile f t = BL.writeFile f $ serializePlan t

planFromFile :: FilePath -> IO (NodeMap [Tag], [AlgNode], NodeMap PFAlgebra)
planFromFile f = liftM deserializePlan $ BL.readFile f
