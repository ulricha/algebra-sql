{-# LANGUAGE FlexibleContexts #-}
module Ferry.Compiler.ExecuteStep where

import Control.Monad.Writer
import Control.Monad.Error
import System.IO
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error


executeStep :: Config -> CompilationStep a b -> a -> PhaseResult b
executeStep opts step i = do
                            phaseHeader (stageName step) (stageMode step) opts
                            b <- (stageStep step) opts i
                            artefactToPhase $ mapM_ (createArtefacts opts b) $ stageArtefacts step
                            -- map logMsg results
                            if (mode opts == stageMode step)
                             then endProcess
                             else return b

createArtefacts :: Config -> b -> (Artefact, String, Handle -> b -> CreateArtefact) -> CreateArtefact
createArtefacts opts i (a, e, f) = do
                                  logMsg line
                                  logMsg $ "Artefact creation stage for: " ++ (show a)
                                  if elem a $ artefact opts
                                   then do 
                                         h <- case output opts of
                                                  Nothing -> return stdout
                                                  (Just file) -> liftIO $ openFile (file ++ "." ++ e) WriteMode 
                                         logMsg "Creating artefact"
                                         f h i
                                         liftIO $ hFlush h
                                         logMsg "Artefact creation done"
                                         logMsg line
                                         return ()
                                   else do
                                         logMsg "Artefact not required. Skipping artefact creation."
                                         logMsg line
                                         return ()
    
phaseHeader :: (MonadWriter [String] m) => Name -> Mode -> Config -> m ()
phaseHeader n s c = do
                     logMsg line
                     logMsg $ "Compiler stage: " ++ (show s)
                     logMsg $ "Stage name: " ++ n
                     logMsg "Compiling with the following options:"
                     logMsg $ show c
                     logMsg line
                     return ()