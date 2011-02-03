{-# LANGUAGE TemplateHaskell #-}
-- | This module wraps the transformation stage from ferry core, to typed ferry core.
module Database.Ferry.Compiler.Stages.TypeInferStage (typeInferPhase) where
    
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.ExecuteStep

import Database.Ferry.Common.Render.Pretty
import Database.Ferry.TypedCore.Data.Type
import Database.Ferry.TypeSystem.Prelude
import Database.Ferry.TypedCore.Render.Dot
import Database.Ferry.Common.Render.Dot
import Database.Ferry.TypedCore.Convert.Specialize

import qualified Database.Ferry.Core.Data.Core as C
import Database.Ferry.TypedCore.Data.TypedCore
import Database.Ferry.TypeSystem.AlgorithmW

import Database.Ferry.Impossible

typeInferPhase :: C.CoreExpr -> PhaseResult CoreExpr
typeInferPhase e = executeStep inferStage e

inferStage :: CompilationStep C.CoreExpr CoreExpr
inferStage = CompilationStep "TypeInfer" TypeInfer step artefacts
    where
        step :: C.CoreExpr -> PhaseResult CoreExpr
        step e = let (res, _) = typeInfer primitives e
                  in case res of
                       Left err -> newError err
                       Right expr -> return $ groupNSpecialize expr
        artefacts = [(Type ,"ty", \s -> return $ prettyPrint $ typeOf s)
                    ,(DotType ,"dot", \s -> return $ makeDot s)]
        
makeDot :: CoreExpr -> String
makeDot c = case runDot $ toDot c of
            Right s -> s
            Left _ -> $impossible
