{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Algebra.Table.Render.JSON
    ( serializePlan
    , deserializePlan
    , planToFile
    , planFromFile
    ) where

import           Control.Monad
import           GHC.Generics                             (Generic)

import           Data.Aeson                               (FromJSON, ToJSON,
                                                           decode, encode)
import qualified Data.ByteString.Lazy.Char8               as BL
import qualified Data.IntMap                              as M

import           Database.Algebra.Dag.Common
import           Database.Algebra.Table.Lang


instance ToJSON ATy where
instance ToJSON AVal where
instance ToJSON SortDir where
instance ToJSON JoinRel where
instance ToJSON SortSpec where
instance ToJSON AggrType where
instance ToJSON NullOp where
instance ToJSON UnOp where
instance ToJSON BinOp where
instance ToJSON Expr where
instance ToJSON UnFun where
instance ToJSON BinFun where
instance ToJSON Key where
instance ToJSON DescrCol where
instance ToJSON SerializeOrder where
instance ToJSON PayloadCol where

instance FromJSON ATy where
instance FromJSON AVal where
instance FromJSON SortDir where
instance FromJSON JoinRel where
instance FromJSON SortSpec where
instance FromJSON AggrType where
instance FromJSON NullOp where
instance FromJSON UnOp where
instance FromJSON BinOp where
instance FromJSON Expr where
instance FromJSON UnFun where
instance FromJSON BinFun where
instance FromJSON Key where
instance FromJSON DescrCol where
instance FromJSON SerializeOrder where
instance FromJSON PayloadCol where

instance ToJSON Plan where
instance FromJSON Plan where

data Plan = Plan { tags :: [(AlgNode, [Tag])], roots :: [AlgNode], graph :: [(AlgNode, TableAlgebra)] }
    deriving Generic

serializePlan :: (NodeMap [Tag], [AlgNode], NodeMap TableAlgebra) -> BL.ByteString
serializePlan (ts, rs, g) = let tags' = M.toList ts
                                graph' = M.toList g
                             in encode $ Plan {tags = tags', roots = rs, graph = graph'}

deserializePlan :: BL.ByteString -> (NodeMap [Tag], [AlgNode], NodeMap TableAlgebra)
deserializePlan s = let Just (Plan ts rs g) = decode s
                     in (M.fromList ts, rs, M.fromList g)

planToFile :: FilePath -> (NodeMap [Tag], [AlgNode], NodeMap TableAlgebra) -> IO ()
planToFile f t = BL.writeFile f $ serializePlan t

planFromFile :: FilePath -> IO (NodeMap [Tag], [AlgNode], NodeMap TableAlgebra)
planFromFile f = liftM deserializePlan $ BL.readFile f
