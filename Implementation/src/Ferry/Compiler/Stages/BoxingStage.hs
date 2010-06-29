module Ferry.Compiler.Stages.BoxingStage (boxingPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.TypedCore.Data.Instances
import Ferry.TypeSystem.Prelude
import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot
                                           
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Boxing.Boxing

boxingPhase :: CoreExpr -> PhaseResult CoreExpr
boxingPhase e = executeStep inferStage e

inferStage :: CompilationStep CoreExpr CoreExpr
inferStage = CompilationStep "Boxing" TypeInfer step artefacts
    where
        step :: CoreExpr -> PhaseResult CoreExpr
        step e = return $ runBoxing primitives e
        artefacts = [(DotBox ,"dot", \s -> return $ makeDot s)]
        
makeDot :: CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left e -> error "Jikes"
