module Ferry.Compiler.Stages.ReadStage (readPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

readPhase :: String -> PhaseResult String
readPhase s = executeStep readStage s

readStage :: CompilationStep String String
readStage = CompilationStep "Read"  Read step artefacts
    where
        step :: String -> PhaseResult String
        step arg = return arg
        artefacts = [(Echo, "echo", \s -> return s)]
        