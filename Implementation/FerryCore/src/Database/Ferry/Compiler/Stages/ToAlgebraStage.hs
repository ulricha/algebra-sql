-- | This module wraps the stage that translates ferry core into an algebraic graph
module Database.Ferry.Compiler.Stages.ToAlgebraStage (algebraPhase) where
    
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.ExecuteStep

import Database.Ferry.Algebra(runGraph, initLoop, AlgPlan)
import Database.Ferry.Common.Data.Plans

import Database.Ferry.TypedCore.Data.Instances()
import Database.Ferry.TypedCore.Convert.CoreToAlgebra
import Database.Ferry.TypedCore.Data.Type
                                           
import Database.Ferry.TypedCore.Data.TypedCore

algebraPhase :: CoreExpr -> PhaseResult (Qual FType, AlgPlan AlgRes)
algebraPhase e = executeStep algebraStage e

algebraStage :: CompilationStep CoreExpr (Qual FType, AlgPlan AlgRes)
algebraStage = CompilationStep "ToAlg" Algebra step artefacts
    where
        step :: CoreExpr -> PhaseResult (Qual FType, AlgPlan AlgRes)
        step e = let eTy = typeOf e
                  in return $ (eTy, runGraph initLoop $ coreToAlgebra e)
        artefacts = []
