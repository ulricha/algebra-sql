module Ferry.Compiler.Pipeline (pipeline, backEndPipeline) where

import Ferry.Compiler.Types
import Ferry.TypedCore.Data.TypedCore (CoreExpr)

import Ferry.Compiler.Stages
    
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