module Ferry.Compiler.Stages.ReadStage (readPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import System.IO

readPhase :: Config -> String -> PhaseResult String
readPhase c s = executeStep c readStage s

readStage :: CompilationStep String String
readStage = CompilationStep "Read"  Read step artefacts
    where
        step :: Config -> String -> PhaseResult String
        step opts arg = return arg
        artefacts = [(Echo, "echo", \h s -> intoArtefact $ hPutStrLn h s)]
        