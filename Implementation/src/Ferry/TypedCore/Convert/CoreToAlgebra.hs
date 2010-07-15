module Ferry.TypedCore.Convert.CoreToAlgebra where

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.Create
import Ferry.Algebra.Data.GraphBuilder

import Ferry.TypedCore.Data.TypedCore

import qualified Data.Map as M 

coreToAlgebra :: CoreExpr -> GraphM (Int, Columns, M.Map k a)
coreToAlgebra = undefined