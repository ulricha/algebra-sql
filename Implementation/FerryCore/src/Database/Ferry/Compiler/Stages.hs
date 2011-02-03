{-| Compiler stages of the backend-}
module Database.Ferry.Compiler.Stages (xmlPhase, boxingPhase, rewritePhase, 
                              algebraPhase, typeInferPhase) where
    
    
import Database.Ferry.Compiler.Stages.AlgebraToXMLStage
import Database.Ferry.Compiler.Stages.BoxingStage
import Database.Ferry.Compiler.Stages.RewriteStage
import Database.Ferry.Compiler.Stages.ToAlgebraStage
import Database.Ferry.Compiler.Stages.TypeInferStage 