{-# LANGUAGE FlexibleContexts #-}
module Ferry.Compiler.Types where
    
import Control.Monad.Error
import Control.Monad.Writer

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
              
type PhaseResult err r = ErrorT err (Writer Log) r

type CompilationStep a b err = (Name, Stage, a -> PhaseResult err b)

type Name = String
type Stage = Int

type Log = [String]

runPhase :: PhaseResult err r -> (Either err r, Log)
runPhase n = runWriter $ runErrorT n 

error :: err -> PhaseResult err r
error e = ErrorT $ return $ Left e

line :: String
line = "--------------------------------------------------"

logMsg :: (MonadWriter [t] m) => t -> m ()
logMsg s = tell [s]

phaseHeader :: (MonadWriter [String] m) => Name -> Stage -> Config -> m ()
phaseHeader n s c = do
                     logMsg line
                     logMsg $ "Compiler stage: " ++ (show s)
                     logMsg $ "Stage name: " ++ n
                     logMsg "Compiling with the following options:"
                     logMsg line
                     return ()
