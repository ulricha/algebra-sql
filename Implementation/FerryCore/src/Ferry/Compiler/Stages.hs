{-| Compiler stages of the backend-}
module Ferry.Compiler.Stages (xmlPhase, boxingPhase, rewritePhase, 
                              algebraPhase, typeInferPhase) where
    
    
import Ferry.Compiler.Stages.AlgebraToXMLStage
import Ferry.Compiler.Stages.BoxingStage
import Ferry.Compiler.Stages.RewriteStage
import Ferry.Compiler.Stages.ToAlgebraStage
import Ferry.Compiler.Stages.TypeInferStage 