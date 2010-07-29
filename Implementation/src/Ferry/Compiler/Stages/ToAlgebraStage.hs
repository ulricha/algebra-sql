module Ferry.Compiler.Stages.ToAlgebraStage (algebraPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder
import Ferry.Algebra.Data.Create

import Ferry.TypedCore.Convert.CoreToAlgebra
import Ferry.TypedCore.Data.Instances
import Ferry.TypeSystem.Prelude
import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot
import Ferry.TypedCore.Data.Type
                                           
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Boxing.Boxing

algebraPhase :: CoreExpr -> PhaseResult (Qual FType, AlgPlan)
algebraPhase e = executeStep algebraStage e

algebraStage :: CompilationStep CoreExpr (Qual FType, AlgPlan)
algebraStage = CompilationStep "ToAlg" Algebra step artefacts
    where
        step :: CoreExpr -> PhaseResult (Qual FType, AlgPlan)
        step e = let eTy = typeOf e
                  in return $ (eTy, runGraph initLoop $ coreToAlgebra e)
        artefacts = []
