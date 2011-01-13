{-# LANGUAGE TemplateHaskell #-}
-- | This module wraps the rewrite stage, performing some rewrites on the ferry core AST.
module Ferry.Compiler.Stages.RewriteStage (rewritePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.ExecuteStep

import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot
                                           
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Rewrite.OpRewrite

import Ferry.Impossible

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
            Left _ -> $impossible