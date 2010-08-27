module Ferry.Compiler.Stages (xmlPhase, boxingPhase, normalisePhase, 
                              parsePhase, readPhase, rewritePhase, 
                              algebraPhase, toCorePhase, typeInferPhase) where
    
    
import Ferry.Compiler.Stages.AlgebraToXMLStage
import Ferry.Compiler.Stages.BoxingStage
import Ferry.Compiler.Stages.NormaliseStage
import Ferry.Compiler.Stages.ParseStage
import Ferry.Compiler.Stages.ReadStage
import Ferry.Compiler.Stages.RewriteStage
import Ferry.Compiler.Stages.ToAlgebraStage
import Ferry.Compiler.Stages.ToCoreStage
import Ferry.Compiler.Stages.TypeInferStage 