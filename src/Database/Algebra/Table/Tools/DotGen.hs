-- | Debugging utility for table algebra plans. Takes a plan from the command
-- line or standard input and renders it into a GraphViz file.
module Main where

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Aeson
import           Data.ByteString.Lazy.Char8        (pack)

import           Data.Maybe

import           Database.Algebra.Dag
import           Database.Algebra.Table.Render.Dot

data Options = Options { optInput     :: IO String
                       , optRootNodes :: Maybe [Int]
                       }

startOptions :: Options
startOptions = Options { optInput            = getContents
                       , optRootNodes        = Nothing
                       }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "i" ["input"]
      (ReqArg (\arg opt -> return opt { optInput = readFile arg })
       "FILE")
      "Input file"
  , Option "n" ["rootnodes"]
      (ReqArg (\arg opt -> return opt { optRootNodes = Just $ read arg })
       "ROOTNODES")
      "List of root nodes to use (must be in Haskell list syntax)"
  , Option "h" ["help"]
      (NoArg
         (\_ -> do
             prg <- getProgName
             hPutStrLn stderr (usageInfo prg options)
             exitWith ExitSuccess))
      "Show help"
  ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, _, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optInput = input
                , optRootNodes = mRootNodes
                } = opts

    plan <- input

    let dag = fromJust $ decode $ pack plan
        rs  = fromMaybe (rootNodes dag) mRootNodes

    let dot = renderTADot rs (nodeMap dag)

    putStr dot
