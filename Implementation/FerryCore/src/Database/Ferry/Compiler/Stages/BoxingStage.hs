{-# LANGUAGE TemplateHaskell #-}
{- | This module wraps the boxing stage. Boxing is performed to ensure that nested lists are 
   handled in a separate table in the database. 
-}
module Database.Ferry.Compiler.Stages.BoxingStage (boxingPhase) where
    
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.ExecuteStep

import Database.Ferry.TypeSystem.Prelude
import Database.Ferry.TypedCore.Render.Dot
import Database.Ferry.Common.Render.Dot
                                           
import Database.Ferry.TypedCore.Data.TypedCore
import Database.Ferry.TypedCore.Boxing.Boxing

import Database.Ferry.Impossible

boxingPhase :: CoreExpr -> PhaseResult CoreExpr
boxingPhase e = executeStep inferStage e

inferStage :: CompilationStep CoreExpr CoreExpr
inferStage = CompilationStep "Boxing" Boxing step artefacts
    where
        step :: CoreExpr -> PhaseResult CoreExpr
        step e = return $ runBoxing primitives e
        artefacts = [(DotBox ,"dot", \s -> return $ makeDot s)]
        
makeDot :: CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left _ -> $impossible
