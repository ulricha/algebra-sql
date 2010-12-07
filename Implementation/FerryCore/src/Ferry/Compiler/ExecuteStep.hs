{-# LANGUAGE FlexibleContexts #-}
{-| The compilation process is build out of small steps, this module provides the infrastructure
that executes one such step. -}
module Ferry.Compiler.ExecuteStep (executeStep) where
    
import Ferry.Compiler.Types

-- | Apply a compilation step to an expression of type a. The result of type b is returned in a phaseresult monad
executeStep :: CompilationStep a b -> a -> PhaseResult b
executeStep step i = do
                            opts <- getConfig
                            phaseHeader (stageName step) (stageMode step)
                            b <- stageStep step i  -- stageStep step gets the function that is to be applied this phase
                            mapM_ (createArtefacts b) $ stageArtefacts step
                            if (mode opts == stageMode step)
                             then endProcess
                             else return b

-- | The artefacts that can be generated by a compilationstep are generated by this function
createArtefacts :: b -> (Artefact, String, b -> ArtefactResult) -> PhaseResult ()
createArtefacts i (a, e, f) = do
                                  opts <- getConfig
                                  logMsg line
                                  logMsg $ "Artefact creation stage for: " ++ (show a)
                                  if elem a $ artefact opts
                                   then do 
                                         let file = case output opts of
                                                  Nothing -> Nothing
                                                  (Just file') -> Just $ file' ++ "." ++ e
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

-- | Helper function to generate the phaseheader in the log    
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