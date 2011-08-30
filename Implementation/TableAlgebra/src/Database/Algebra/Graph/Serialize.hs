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
rootsFromFile f = liftM deserializeRoots $ readFile f

deserializeRoots :: String -> [AlgNode]
deserializeRoots s = read s :: [AlgNode]

rootsToFile :: FilePath -> [AlgNode] -> IO ()
rootsToFile f rs = writeFile f $ serializeRoots rs

serializeRoots :: [AlgNode] -> String
serializeRoots ns = show ns
