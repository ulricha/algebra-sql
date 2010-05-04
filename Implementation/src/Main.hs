module Main where

import System.Console.GetOpt
import System.Environment
import Ferry.Front.Parser.Parser


data Config = Config {
              mode :: Mode,
              debug :: Bool
            }

data Mode = Echo | Parse | Pretty

defaultConfig :: Config
defaultConfig = Config {
                mode        = Parse,
                debug       = False
              }

options :: [OptDescr (Config -> Config)]
options = [ Option ['e'] ["echo"]
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

processArgs :: String -> [String] -> IO (Config, [String])
processArgs progName args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ "\n" ++ (concat errors) ++ usageInfo header options
  where
    header = "\nUsage: " ++ progName
             ++ " [OPTION...] [FILES], with the following options:"

main :: IO ()
main = 
    do
        args <- getArgs
        progName <- getProgName
        (opts, fileNames) <- processArgs progName args
        compile opts (head fileNames)

compile :: Config -> String -> IO ()
compile opts file = do
                        src <- readFile file
                        let ast = parseFerry file src
                        putStrLn $ show ast
                        return ()
                        --putStrLn (show $ length tokens)  

