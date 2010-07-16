module Ferry.Compiler.Stages.AlgebraToXMLStage (xmlPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder
import Ferry.Algebra.Render.XML

xmlPhase :: AlgPlan -> PhaseResult String
xmlPhase e = executeStep xmlStage e

xmlStage :: CompilationStep AlgPlan String
xmlStage = CompilationStep "ToXML" AlgebraXML step artefacts
    where
        step :: AlgPlan -> PhaseResult String
        step = return . show . transform 
        artefacts = [(XML, "xml", return)]
