-- | This module wraps the stage that translates ferry core into an algebraic graph
module Ferry.Compiler.Stages.ToAlgebraStage (algebraPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra(runGraph, initLoop, AlgPlan)

import Ferry.TypedCore.Data.Instances()
import Ferry.TypedCore.Convert.CoreToAlgebra
import Ferry.TypedCore.Data.Type
                                           
import Ferry.TypedCore.Data.TypedCore

algebraPhase :: CoreExpr -> PhaseResult (Qual FType, AlgPlan)
algebraPhase e = executeStep algebraStage e

algebraStage :: CompilationStep CoreExpr (Qual FType, AlgPlan)
algebraStage = CompilationStep "ToAlg" Algebra step artefacts
    where
        step :: CoreExpr -> PhaseResult (Qual FType, AlgPlan)
        step e = let eTy = typeOf e
                  in return $ (eTy, runGraph initLoop $ coreToAlgebra e)
        artefacts = []
