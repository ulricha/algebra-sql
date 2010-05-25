{-# LANGUAGE FlexibleContexts #-}
module Ferry.Compiler.ExecuteStep where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error


executeStep :: CompilationStep a b -> a -> PhaseResult b
executeStep step i = do
                            opts <- getConfig
                            phaseHeader (stageName step) (stageMode step)
                            b <- stageStep step i
                            c <- getConfig
                            mapM_ (createArtefacts b) $ stageArtefacts step
                            if (mode opts == stageMode step)
                             then endProcess
                             else return b

createArtefacts :: b -> (Artefact, String, b -> ArtefactResult) -> PhaseResult ()
createArtefacts i (a, e, f) = do
                                  opts <- getConfig
                                  logMsg line
                                  logMsg $ "Artefact creation stage for: " ++ (show a)
                                  if elem a $ artefact opts
                                   then do 
                                         let file = case output opts of
                                                  Nothing -> Nothing
                                                  (Just file) -> Just $ file ++ "." ++ e
                                         logMsg "Creating artefact"
                                         s <- artefactToPhaseResult $ f i
                                         addFile file s
                                         logMsg "Artefact creation done"
                                         logMsg line
                                         return ()
                                   else do
                                         logMsg "Artefact not required. Skipping artefact creation."
                                         logMsg line
                                         return ()
    
phaseHeader :: Name -> Mode -> PhaseResult ()
phaseHeader n s = do
                     c <- getConfig
                     logMsg line
                     logMsg $ "Compiler stage: " ++ (show s)
                     logMsg $ "Stage name: " ++ n
                     logMsg "Compiling with the following options:"
                     logMsg $ show c
                     logMsg line
                     return ()