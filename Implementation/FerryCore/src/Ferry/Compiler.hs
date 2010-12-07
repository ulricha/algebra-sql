-- | The compiler interface
module Ferry.Compiler
( module Ferry.Compiler.Stages,
  FerryError(..), handleError,
  typedCoreToAlgebra,
  module Ferry.Compiler.Types,
  backEndPipeline, backEndPipeline',
  executeStep 
) where
    
import Ferry.Compiler.Error.Error (FerryError (..), handleError)
import Ferry.Compiler.Stages
import Ferry.Compiler.Transform
import Ferry.Compiler.Types
import Ferry.Compiler.Pipeline
import Ferry.Compiler.ExecuteStep