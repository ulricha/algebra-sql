-- | Normalise the parsed result, removes all constructs that cannot be handled by ferry core
module Ferry.Compiler.Stages.NormaliseStage (normalisePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.ExecuteStep

import Ferry.Front.Convert.Normalise
import Ferry.Front.Data.Language
import Ferry.Front.Render.Pretty()
import Ferry.Common.Render.Pretty


normalisePhase :: Expr -> PhaseResult Expr
normalisePhase e = executeStep normaliseStage e

normaliseStage :: CompilationStep Expr Expr
normaliseStage = CompilationStep "Normalise" Normalise step artefacts
    where
        step :: Expr -> PhaseResult Expr
        step e = let res = runNormalisation e
                  in case res of
                       Left err -> newError err
                       Right expr -> return expr
        artefacts = [(PrettyNormalAST, "norm.ferry", \s -> return $ prettyPrint s)]