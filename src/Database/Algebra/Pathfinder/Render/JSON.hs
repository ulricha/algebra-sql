--JSON
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module Database.Algebra.Pathfinder.Render.JSON(serializePlan, deserializePlan, planToFile, planFromFile) where

import GHC.Generics (Generic)    
import Control.Monad

import Database.Algebra.Dag.Common
import Database.Algebra.Pathfinder.Data.Algebra 
import qualified Data.IntMap as M
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (FromJSON, ToJSON, decode, encode)


instance ToJSON ATy where
instance ToJSON AVal where
instance ToJSON SortDir where
instance ToJSON JoinRel where
instance ToJSON SortInf where
instance ToJSON AggrType where
instance ToJSON NullOp where
instance ToJSON UnOp where
instance ToJSON BinOp where
instance ToJSON Expr where
instance ToJSON UnFun where
instance ToJSON BinFun where
instance ToJSON Key where

instance FromJSON ATy where
instance FromJSON AVal where
instance FromJSON SortDir where
instance FromJSON JoinRel where
instance FromJSON SortInf where
instance FromJSON AggrType where
instance FromJSON NullOp where
instance FromJSON UnOp where
instance FromJSON BinOp where
instance FromJSON Expr where
instance FromJSON UnFun where
instance FromJSON BinFun where
instance FromJSON Key where

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
