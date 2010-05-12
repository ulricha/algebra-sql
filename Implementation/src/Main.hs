-- | This class forms the main executable unit of the Ferry 2.0 compiler
module Main where

import System.Console.GetOpt
import System.Environment

import Ferry.Compiler.Types
import Ferry.Compiler.Compile

stopSelect :: String -> Mode
stopSelect n = if n == "1"
                 then Read
                 else if n == "2"
                 then Parse
                 else if n == "3"
                 then Normalise
                 else if n == "10"
                 then Transform
                 else Full
                 
-- | Description of the options for the compiler 'Mode'
options :: [OptDescr (Config -> Config)]
options = [ Option ['s'] ["stop"]
                   (ReqArg (\s o -> o {mode = stopSelect s}) "PHASE")
                   "Stop after phase"
          , Option ['r'] ["read"]
                   (NoArg (\o -> o {mode = Read}))
                   "Read source file"
          , Option ['c'] ["core"]
                   (NoArg (\o -> o {mode = Transform}))
                   "Transform to core"
          , Option ['o']["out"]
                   (ReqArg (\s o -> o {output = Just s}) "FILE")
                   "Direct output to files starting with FILE"
          , Option ['l', 'd']["log", "debug"]
                   (OptArg (\s o -> o {debug = True, logFile = s}) "LOGFILE")
                   "Debug output, when file is specified direct log to LOGFILE"
          , Option ['a']["all"]
                   (NoArg (\o -> o {mode = Full, artefact = allArtefacts}))
                   "Generate all artefacts"
          , Option []["dotAST"]
                   (NoArg (\o -> o {artefact=DotAST:(artefact o)}))
                   "Generate dot file for AST"
          , Option []["dotCore"]
                   (NoArg (\o -> o {artefact=DotCore:(artefact o)}))
                   "Generate dot file for Core"
          , Option []["prettyCore"]
                   (NoArg (\o -> o {artefact=PrettyCore:(artefact o)}))
                   "Pretty print core program"
          , Option ['i'] ["input"]
                   (NoArg (\o -> o {input = Arg}))
                   "Input through stdin"                   
          , Option ['e'] ["echo"]
                   (NoArg (\o -> o {artefact = Echo : (artefact o)}))
                   "Print program to screen."
          , Option ['p'] ["parse"]
                   (NoArg (\o -> o {mode = Parse}))
                   "Parse program"
          , Option ['n'] ["prettyAst"]
                    (NoArg (\o -> o {artefact = PrettyAST : (artefact o)}))
                       "Parse program, pretty print AST."
          ]

-- | Process the arguments given to the compiler
processArgs :: String -> [String] -> IO (Config, [String])
processArgs progName args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ "\n" ++ (concat errors) ++ usageInfo header options
  where
    header = "\nUsage: " ++ progName
             ++ " [OPTION...] [FILE], with the following options:"

-- | Main thread
main :: IO ()
main = 
    do
        args <- getArgs
        progName <- getProgName
        (opts, inp) <- processArgs progName args
        compile opts inp



