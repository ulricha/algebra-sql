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


toCorePhase :: Config -> Expr -> PhaseResult C.CoreExpr
toCorePhase c e = executeStep c normaliseStage e

normaliseStage :: CompilationStep Expr C.CoreExpr
normaliseStage = CompilationStep "FrontToCore" Transform step artefacts
    where
        step :: Config -> Expr -> PhaseResult C.CoreExpr
        step opts e = let res = runTransformation e
                       in case res of
                             Left err -> newError err
                             Right expr -> return expr
        artefacts = [(DotCore ,"dot", \h s -> intoArtefact $ hPutStrLn h $ makeDot s)]
        
makeDot :: C.CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left e -> error "Jikes"