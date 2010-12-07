-- | This module is exposed in the library allowing other applications to compile typedCore to relational algebra
{-# LANGUAGE TemplateHaskell #-}
module Ferry.Compiler.Transform (typedCoreToAlgebra) where
    
import Ferry.Compiler.Pipeline (backEndPipeline')
import Ferry.TypedCore.Data.TypedCore (CoreExpr)
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Impossible

typedCoreToAlgebra :: CoreExpr -> String
typedCoreToAlgebra = compile defaultConfig 

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> CoreExpr -> String
compile opts inp = do
                        let (r, _, f) = runPhase opts $ backEndPipeline' inp   
                        case (r, f) of
                            (Right (), [(_, o)]) -> o
                            (Left ProcessComplete, [(_, o)]) -> o
                            (Left err, _)       -> error $ show err
                            _                   -> $impossible
                            