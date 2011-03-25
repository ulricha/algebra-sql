{- | This module wraps the transform algebra into xml compilation stage
-}
module Database.Ferry.Compiler.Stages.AlgebraToXMLStage (xmlPhase) where
    
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.ExecuteStep

import Database.Ferry.Common.Data.Plans
import Database.Ferry.Algebra(AlgPlan)

import Database.Ferry.TypedCore.Data.Type


xmlPhase :: (Qual FType, AlgPlan AlgRes) -> PhaseResult String
xmlPhase (_ :=> t, p) = executeStep xmlStage $ case t of
                                                FList _ -> (True, False, p)
                                                _       -> (False, False, p)

xmlStage :: CompilationStep (Bool, Bool, AlgPlan AlgRes) String
xmlStage = CompilationStep "ToXML" AlgebraXML step artefacts
    where
        step :: (Bool, Bool, AlgPlan AlgRes) -> PhaseResult String
        step = return . show . transform 
        artefacts = [(XML, "xml", return)]