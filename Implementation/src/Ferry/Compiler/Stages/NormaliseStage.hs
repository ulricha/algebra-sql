module Ferry.Compiler.Stages.NormaliseStage (normalisePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Front.Convert.Normalise
import Ferry.Front.Data.Language
import Ferry.Front.Render.Pretty

import System.IO


normalisePhase :: Config -> Expr -> PhaseResult Expr
normalisePhase c e = executeStep c normaliseStage e

normaliseStage :: CompilationStep Expr Expr
normaliseStage = CompilationStep "Normalise" Normalise step artefacts
    where
        step :: Config -> Expr -> PhaseResult Expr
        step opts e = let res = runNormalisation e
                       in case res of
                             Left err -> newError err
                             Right expr -> return expr
        artefacts = [(PrettyNormalAST, "norm.ferry", \h s -> intoArtefact $ hPutStr h $  prettyPrint s)]