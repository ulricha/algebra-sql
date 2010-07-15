module Ferry.Compiler.Stages.ToAlgebraStage (algebraPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.GraphBuilder

import Ferry.TypedCore.Convert.CoreToAlgebra
import Ferry.TypedCore.Data.Instances
import Ferry.TypeSystem.Prelude
import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot
                                           
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Boxing.Boxing

algebraPhase :: CoreExpr -> PhaseResult AlgGr
algebraPhase e = executeStep algebraStage e

algebraStage :: CompilationStep CoreExpr AlgGr
algebraStage = CompilationStep "ToAlg" Algebra step artefacts
    where
        step :: CoreExpr -> PhaseResult AlgGr
        step e = return $ runGraph $ coreToAlgebra e
        artefacts = []
