{-# LANGUAGE TemplateHaskell #-}
-- | This module wraps the rewrite stage, performing some rewrites on the ferry core AST.
module Database.Ferry.Compiler.Stages.RewriteStage (rewritePhase) where
    
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.ExecuteStep

import Database.Ferry.TypedCore.Render.Dot
import Database.Ferry.Common.Render.Dot
                                           
import Database.Ferry.TypedCore.Data.TypedCore
import Database.Ferry.TypedCore.Rewrite.OpRewrite

import Database.Ferry.Impossible

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
