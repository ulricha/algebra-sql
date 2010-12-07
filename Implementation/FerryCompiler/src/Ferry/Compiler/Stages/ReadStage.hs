-- | This modules wraps the read stage, read the code from a source file to an in memory string
module Ferry.Compiler.Stages.ReadStage (readPhase) where
    
import Ferry.Compiler

readPhase :: String -> PhaseResult String
readPhase s = executeStep readStage s

readStage :: CompilationStep String String
readStage = CompilationStep "Read"  Read step artefacts
    where
        step :: String -> PhaseResult String
        step arg = return arg
        artefacts = [(Echo, "echo", \s -> return s)]
        