-- | This module wrap the to core stage, transforming ferry front into ferry core
module Ferry.Compiler.Stages.ToCoreStage (toCorePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Common.Render.Dot
import Ferry.Core.Render.Dot

import Ferry.Front.Convert.FrontToCore
import Ferry.Front.Data.Language
import qualified Ferry.Core.Data.Core as C
import System.IO


toCorePhase :: Expr -> PhaseResult C.CoreExpr
toCorePhase e = executeStep normaliseStage e

normaliseStage :: CompilationStep Expr C.CoreExpr
normaliseStage = CompilationStep "FrontToCore" Transform step artefacts
    where
        step :: Expr -> PhaseResult C.CoreExpr
        step e = let res = runTransformation e
                  in case res of
                       Left err -> newError err
                       Right expr -> return expr
        artefacts = [(DotCore ,"dot", \s -> return $ makeDot s)]
        
makeDot :: C.CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left e -> error "Jikes"