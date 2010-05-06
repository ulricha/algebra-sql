-- | This class forms the main executable unit of the Ferry 2.0 compiler
module Main where

import System.Console.GetOpt
import System.Environment
import Ferry.Front.Parser.Parser
import System.FilePath.Posix(takeFileName)

-- | The config datatype is used to store program flags given by the user 
--   The compiler can be put in a 'Mode' that determines what sort of
--   result the compilation process will result in.
--   The 'Input' element is set to specify whether a file should be compiled or 
--   input from the stdin
--   The debug component is set to switch on debugging mode, debugging mode
--   results in log information on the stdin and possibly extra compiler artifacts.
data Config = Config {
              mode :: Mode,
              input :: Input,
              debug :: Bool
            }

-- | The modes that are supported by the compiler.
--   run ferryc -h to see a list of all options
data Mode = Echo   -- ^ Echo mode prints the given input to the console
          | Parse  -- ^ Parse mode will stop the compiler after the parsing phase
          | Pretty -- ^ Pretty mode parses the given input and pretty prints the result

-- | The input mode determines whether the source program is given through a file or via stdin
data Input  = File -- ^ File mode, the program is read from a file 
            | Arg  -- ^ Argument mode, the program is given as input directly

-- | The default configuration for the compiler
defaultConfig :: Config
defaultConfig = Config {
                --  Standard 'Mode' is set to Parse
                mode        = Parse, 
                --  By default the program is given through a File
                input       = File, 
                --  Debug turned of by default
                debug       = False 
              }

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

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> [String] -> IO ()
compile opts inp = do
                        src <- case (input opts) of
                                File -> readFile $ head inp
                                Arg  -> return $ unlines inp
                        let file = case (input opts) of
                                    File -> takeFileName $ head inp
                                    Arg  -> "StdIn"
                        let ast = parseFerry file src
                        putStrLn $ show ast
                        return ()
                        --putStrLn (show $ length tokens)  

