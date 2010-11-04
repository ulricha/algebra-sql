-- | This class forms the main executable unit of the Ferry 2.0 compiler
module Main where

import System.Console.GetOpt
import System.Environment

import Ferry.Compiler
import Ferry.Compiler.Compile

-- | The stop select function is used to determine at what part of the
-- | compilation chain the compiler needs to stop. All requested artefacts
-- | until that stage will be generated
stopSelect :: String -> Mode
stopSelect "1"  = Read
stopSelect "2"  = Parse
stopSelect "3"  = Normalise
stopSelect "10" = Transform
stopSelect "20" = TypeInfer
stopSelect "25" = OpRewrite  
stopSelect "30" = Boxing
stopSelect "40" = Algebra
stopSelect _    = AlgebraXML

-- | Synonym for the ferry logo type
type Logo = [String]

-- | Logo of the ferry compiler
logo :: Logo
logo = [
        "     _/_/_/_/                                       ",
        "    _/        _/_/    _/  _/_/  _/  _/_/  _/    _/  ",
        "   _/_/_/  _/_/_/_/  _/_/      _/_/      _/    _/   ",
        "  _/      _/        _/        _/        _/    _/    ",
        " _/        _/_/_/  _/        _/          _/_/_/     ",
        "                                            _/      ",
        "                                       _/_/         ",
        " Ferry Compiler",
        " Developed at Universitaet Tuebingen"
       ]
                                        
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
                   (NoArg (\o -> o {mode = AlgebraXML, artefact = allArtefacts}))
                   "Generate all artefacts"
          , Option []["dotAST"]
                   (NoArg (\o -> o {artefact=DotAST:(artefact o)}))
                   "Generate dot file for AST"
          , Option []["dotCore"]
                   (NoArg (\o -> o {artefact=DotCore:(artefact o)}))
                   "Generate dot file for Core"
          , Option []["dotType"]
                   (NoArg (\o -> o {artefact=DotType:(artefact o)}))
                   "Generate dot file for Typed Core"
          , Option []["dotRewrite"]
                   (NoArg (\o -> o {artefact=DotRewrite:(artefact o)}))
                   "Generate dot file for rewritten Typed Core AST"
          , Option []["dotBox"]
                    (NoArg (\o -> o {artefact=DotBox:(artefact o)}))
                    "Generate dot file for Boxed Core"
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
          , Option [] ["prettyNorm"]
                     (NoArg (\o -> o {artefact = PrettyNormalAST : (artefact o)}))
                        "Parse program, pretty print normalised AST."
          , Option ['f']["file"]
                   (ReqArg (\s o -> o {input = File s}) "File")
                   "Source file input"
          , Option ['t']["type"]
                    (NoArg (\o -> o {artefact = Type : (artefact o)}))
                        "Typecheck program and return its type"
          ]

-- | Process the arguments given to the compiler
processArgs :: String -> [String] -> IO (Config, [String])
processArgs progName args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ "\n" ++ (concat errors) ++ usageInfo header options
  where
    header = "\n" ++ unlines logo ++ "\nUsage: " ++ progName
             ++ " [OPTION...] [FILE], with the following options:"

-- | Main thread
-- | Processes command lines arguments, and then based on these settings starts the compilation process
main :: IO ()
main = 
    do
        args <- getArgs
        progName <- getProgName
        (opts, inp) <- processArgs progName args
        compile opts inp



