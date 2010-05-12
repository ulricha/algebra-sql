module Ferry.Compiler.Stages.ReadStage where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error

readStage :: CompilationStep String String FerryError
readStage = CompilationStep "Read"  Read step artefacts
    where
        step :: Config -> String -> PhaseResult FerryError String
        step opts arg = arg
        artefacts = [(Echo, \s -> putStrLn s)]
        