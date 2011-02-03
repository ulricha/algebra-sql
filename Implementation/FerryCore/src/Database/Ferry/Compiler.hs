-- | The compiler interface
module Database.Ferry.Compiler
( module Database.Ferry.Compiler.Stages,
  FerryError(..), handleError,
  typedCoreToAlgebra,
  module Database.Ferry.Compiler.Types,
  backEndPipeline, backEndPipeline',
  executeStep 
) where
    
import Database.Ferry.Compiler.Error.Error (FerryError (..), handleError)
import Database.Ferry.Compiler.Stages
import Database.Ferry.Compiler.Transform
import Database.Ferry.Compiler.Types
import Database.Ferry.Compiler.Pipeline
import Database.Ferry.Compiler.ExecuteStep