-- | This class forms the main executable unit of the Ferry 2.0 compiler
module Main where

import System.Console.GetOpt
import System.Environment

import Ferry.Compiler.Types
import Ferry.Compiler.Compile

-- | Description of the options for the compiler 'Mode'
options :: [OptDescr (Config -> Config)]
options = [ Option ['i'] ["input"]
                   (NoArg (\o -> o {input = Arg}))
                   "Input through stdin"                   
          , Option ['e'] ["echo"]
                   (NoArg (\o -> o {mode = Echo}))
                   "Print program to screen."
          , Option ['p'] ["parse"]
                   (NoArg (\o -> o {mode = Parse}))
                   "Parse program, print AST."
          , Option ['n'] ["pretty"]
                    (NoArg (\o -> o {mode = Pretty}))
                       "Parse program, pretty print AST."
          , Option ['d'] ["debug"]
                    (NoArg (\o -> o {debug = True}))
                    "Print debugging information from heuristics."
          ]

-- | Process the arguments given to the compiler
processArgs :: String -> [String] -> IO (Config, [String])
processArgs progName args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ "\n" ++ (concat errors) ++ usageInfo header options
  where
    header = "\nUsage: " ++ progName
             ++ " [OPTION...] [FILES], with the following options:"

-- | Main thread
main :: IO ()
main = 
    do
        args <- getArgs
        progName <- getProgName
        (opts, inp) <- processArgs progName args
        compile opts inp



