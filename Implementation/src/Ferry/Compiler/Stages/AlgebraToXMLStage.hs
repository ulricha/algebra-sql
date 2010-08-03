{- | This module wraps the transform algebra into xml compilation stage
-}

module Ferry.Compiler.Stages.AlgebraToXMLStage (xmlPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder
import Ferry.Algebra.Render.XML
import Ferry.TypedCore.Data.Type

xmlPhase :: (Qual FType, AlgPlan) -> PhaseResult String
xmlPhase e = executeStep xmlStage e

xmlStage :: CompilationStep (Qual FType, AlgPlan) String
xmlStage = CompilationStep "ToXML" AlgebraXML step artefacts
    where
        step :: (Qual FType, AlgPlan) -> PhaseResult String
        step = return . show . transform 
        artefacts = [(XML, "xml", return)]
