module Ferry.Compiler.Pipeline (pipeline, backEndPipeline) where

import Ferry.Compiler.Types
import Ferry.TypedCore.Data.TypedCore (CoreExpr)

import Ferry.Compiler.Stages.ReadStage
import Ferry.Compiler.Stages.ParseStage
import Ferry.Compiler.Stages.NormaliseStage
import Ferry.Compiler.Stages.ToCoreStage
import Ferry.Compiler.Stages.TypeInferStage
import Ferry.Compiler.Stages.RewriteStage
import Ferry.Compiler.Stages.BoxingStage
import Ferry.Compiler.Stages.ToAlgebraStage
import Ferry.Compiler.Stages.AlgebraToXMLStage
    
-- | The compiler pipeline. The given string is transformed dependent on the configuration of the Phaseresult
--   monad.
pipeline :: String -> PhaseResult ()
pipeline src = readPhase src >>=
                 parsePhase >>=
                 normalisePhase >>=
                 toCorePhase >>=
                 typeInferPhase >>=
                 backEndPipeline
                 


backEndPipeline :: CoreExpr -> PhaseResult ()
backEndPipeline c = rewritePhase c >>=
                     boxingPhase >>=
                     algebraPhase >>=
                     xmlPhase >>
                     return ()