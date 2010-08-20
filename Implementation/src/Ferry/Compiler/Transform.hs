module Ferry.Compiler.Transform (transform) where
    
import Ferry.Compiler.Pipeline (backEndPipeline)
import Ferry.TypedCore.Data.TypedCore (CoreExpr)
import Ferry.Compiler.Types

transform :: CoreExpr -> String
transform = compile defaultConfig 

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> CoreExpr -> String
compile opts inp = do
                        let (r, _, f) = runPhase opts $ backEndPipeline inp   
                        case (r, f) of
                            (Right (), [(_, o)]) -> o
                            (Left r, _)         -> error $ show r