module Ferry.Compiler.Pipeline (backEndPipeline, backEndPipeline') where

import Ferry.Compiler.Types
import Ferry.Core.Data.Core (CoreExpr)
import qualified Ferry.TypedCore.Data.TypedCore as T (CoreExpr)

import Ferry.Compiler.Stages
    
-- | The compiler pipeline. The given Core AST is transformed dependent on the configuration of the Phaseresult
--   monad.
backEndPipeline :: CoreExpr -> PhaseResult ()
backEndPipeline c =  typeInferPhase c >>=
                     rewritePhase >>=
                     boxingPhase >>=
                     algebraPhase >>=
                     xmlPhase >>
                     return ()
                     
-- | The compiler pipeline. The given Core AST is transformed dependent on the configuration of the Phaseresult
--   monad.
backEndPipeline' :: T.CoreExpr -> PhaseResult ()
backEndPipeline' c = rewritePhase c >>=
                     boxingPhase >>=
                     algebraPhase >>=
                     xmlPhase >>
                     return ()                     
