module Ferry.Compiler.Stages.TypeInferStage (typeInferPhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Common.Render.Pretty
import Ferry.TypedCore.Render.Pretty
import Ferry.TypedCore.Data.Instances
import Ferry.TypedCore.Data.Type
import Ferry.TypeSystem.Prelude
import Ferry.TypedCore.Render.Dot
import Ferry.Common.Render.Dot

import qualified Ferry.Core.Data.Core as C
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypeSystem.AlgorithmW

import System.IO.Unsafe
import qualified Data.Map as M

typeInferPhase :: C.CoreExpr -> PhaseResult CoreExpr
typeInferPhase e = executeStep inferStage e

inferStage :: CompilationStep C.CoreExpr CoreExpr
inferStage = CompilationStep "TypeInfer" TypeInfer step artefacts
    where
        step :: C.CoreExpr -> PhaseResult CoreExpr
        step e = let (res, s) = typeInfer primitives e
                  in case res of
                       Left err -> newError err
                       Right expr -> return expr
        artefacts = [(Type ,"ty", \s -> return $ prettyPrint $ typeOf s)
                    ,(DotType ,"dot", \s -> return $ makeDot s)]
        
makeDot :: CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left e -> error "Jikes"
