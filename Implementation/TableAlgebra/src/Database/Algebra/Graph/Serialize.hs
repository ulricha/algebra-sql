module Database.Algebra.Graph.Serialize where

import Control.Monad

import Data.Map as Map

import Database.Algebra.Graph.Common

tagsFromFile :: FilePath -> IO Tags
tagsFromFile = liftM deserializeTags . readFile

tagsToFile :: FilePath -> Tags -> IO ()
tagsToFile f ts = writeFile (f ++ ".tags") $ serializeTags ts

serializeTags :: Tags -> String
serializeTags = show . Map.toList

deserializeTags :: String -> Tags
deserializeTags s = Map.fromList (read s :: [(AlgNode, [String])])

rootsFromFile :: FilePath -> IO [AlgNode]
rootsFromFile = liftM (\s -> read s :: [AlgNode]) . readFile

rootsToFile :: FilePath -> [AlgNode] -> IO ()
rootsToFile f ns = writeFile (f ++ ".roots") $ show ns
