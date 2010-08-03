-- | This module wraps the rewrite stage, performing some rewrites on the ferry core AST.
module Ferry.Compiler.Stages.RewriteStage (rewritePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.TypedCore.Data.Instances
import Ferry.TypeSystem.Prelude
import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot
                                           
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Rewrite.OpRewrite

rewritePhase :: CoreExpr -> PhaseResult CoreExpr
rewritePhase e = executeStep inferStage e

inferStage :: CompilationStep CoreExpr CoreExpr
inferStage = CompilationStep "Rewrite" OpRewrite step artefacts
    where
        step :: CoreExpr -> PhaseResult CoreExpr
        step e = return $ rewrite e
        artefacts = [(DotRewrite ,"dot", \s -> return $ makeDot s)]
        
makeDot :: CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left e -> error "Jikes"
