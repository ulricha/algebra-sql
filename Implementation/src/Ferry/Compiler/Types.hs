{-# LANGUAGE FlexibleContexts #-}
module Ferry.Compiler.Types where
    
import Control.Monad.Error
import Control.Monad.Writer
import System.IO

import Ferry.Compiler.Error.Error

-- | The config datatype is used to store program flags given by the user 
--   The compiler can be put in a 'Mode' that determines what sort of
--   result the compilation process will result in.
--   The 'Input' element is set to specify whether a file should be compiled or 
--   input from the stdin
--   The debug component is set to switch on debugging mode, debugging mode
--   results in log information on the stdin and possibly extra compiler artifacts.
data Config = Config {
              mode :: Mode,
              logFile :: Maybe String,
              output :: Maybe String,
              input :: Input,
              artefact :: [Artefact],
              debug :: Bool
            }

-- | The modes that are supported by the compiler.
--   run ferryc -h to see a list of all options
data Mode = Read
          | Parse  -- ^ Parse mode will stop the compiler after the parsing phase
          | Normalise 
          | Transform
          | Full
    deriving (Show, Eq)
          
data Artefact = Echo   -- ^ Echo mode prints the given input to the console
              | PrettyAST -- ^ Pretty mode parses the given input and pretty prints the result
              | PrettyCore
              | DotAST
              | DotCore
              | Algebra
    deriving (Show, Eq)

allArtefacts = [Echo, PrettyAST, PrettyCore, DotAST, DotCore, Algebra]
-- | The input mode determines whether the source program is given through a file or via stdin
data Input  = File String-- ^ File mode, the program is read from a file 
            | Arg  -- ^ Argument mode, the program is given as input directly
    deriving (Show, Eq)

-- | The default configuration for the compiler
defaultConfig :: Config
defaultConfig = Config {
                --  Standard 'Mode' is set to Full
                mode        = Full,
                logFile     = Nothing,
                output      = Nothing, 
                --  By default the program is given through a File
                input       = Arg,
                -- Standard output is the empty list, denoting regular compilation proces
                artefact    = [Echo, Algebra], 
                --  Debug turned of by default
                debug       = False 
              }
              
type PhaseResult r = ErrorT FerryError (WriterT Log IO) r

type CreateArtefact = WriterT Log IO () 

data CompilationStep a b  = CompilationStep { 
                                stageName :: Name, 
                                stageMode :: Mode,
                                stageStep :: Config -> a -> PhaseResult b,
                                stageArtefacts :: [(Artefact, String, Handle -> b -> CreateArtefact)]
                                }

type Name = String
type Stage = Int

type Log = [String]

getLog :: PhaseResult r -> IO Log
getLog n = liftM snd $ runPhase n
            
artefactToPhase :: CreateArtefact -> PhaseResult ()
artefactToPhase a = lift a

intoPhase :: IO a -> PhaseResult a
intoPhase = liftIO

intoArtefact :: IO () -> CreateArtefact
intoArtefact = liftIO

runPhase :: PhaseResult r -> IO (Either FerryError r, Log)
runPhase n = runWriterT $ runErrorT n

newError :: FerryError -> PhaseResult r
newError e = ErrorT $ return $ Left e

endProcess :: PhaseResult b
endProcess = do
                logMsg line
                logMsg "Reached compilation target"
                logMsg "Quiting compilation"
                logMsg line
                newError ProcessComplete

line :: String
line = "--------------------------------------------------"

logMsg :: (MonadWriter [t] m) => t -> m ()
logMsg s = tell [s]


