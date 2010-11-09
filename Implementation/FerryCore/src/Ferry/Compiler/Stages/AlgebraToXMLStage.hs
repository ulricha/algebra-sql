{- | This module wraps the transform algebra into xml compilation stage
-}

module Ferry.Compiler.Stages.AlgebraToXMLStage (xmlPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra(AlgPlan, transform)

import Ferry.TypedCore.Data.Type

xmlPhase :: (Qual FType, AlgPlan) -> PhaseResult String
xmlPhase (_ :=> t, p) = executeStep xmlStage $ case t of
                                                FList _ -> (True, p)
                                                _       -> (False, p)

xmlStage :: CompilationStep (Bool, AlgPlan) String
xmlStage = CompilationStep "ToXML" AlgebraXML step artefacts
    where
        step :: (Bool, AlgPlan) -> PhaseResult String
        step = return . show . transform 
        artefacts = [(XML, "xml", return)]
