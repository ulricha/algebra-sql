module Database.Algebra.Graph.Normalize(normalizePlan, normalizePlanM) where

import Control.Monad.State

import Database.Algebra.Aux
import Database.Algebra.Graph.AlgebraDag
import Database.Algebra.Graph.Common
import Database.Algebra.X100.Data.Algebra
import Database.Algebra.X100.Properties.Types
import Database.Algebra.X100.Properties.BottomUp

{-

Functionality to normalize certain aspects of X100 DAG plans.

At the moment:

1. Guarantee that every query root node has no parents.

2. Remove all unreferenced nodes from the DAG

-}

orphanizeRootNodes :: NodeMap BottomUpProps -> [AlgNode] -> DagRewrite X100Algebra ()
orphanizeRootNodes props rootNodes = mapM_ orphanize rootNodes
    where orphanize root = do
              parentNodes <- parentsM root
              op <- operatorM root
              case parentNodes of
                  _ : _ -> do
                      newNode <- insertM op
                      let s = schemaProp $ lookupUnsafe props "schema" root
                          schemaProjection = map (\(c, _) -> ColProj c) s
                          newRootOp = UnOp (Project schemaProjection) newNode
                      replaceM root newRootOp
                      mapM_ (\p -> replaceChildM p root newNode) parentNodes
                  [] -> return ()


normalizePlanM :: [AlgNode] -> DagRewrite X100Algebra ()
normalizePlanM rootNodes = do
    topOrdering <- topsortM
    propMap <- inferM (inferBottomUpProperties topOrdering)
    orphanizeRootNodes propMap rootNodes
    pruneUnusedM rootNodes

normalizePlan :: [AlgNode] -> AlgebraDag X100Algebra -> AlgebraDag X100Algebra
normalizePlan rootNodes d =
    dag $ execState (normalizePlanM rootNodes) (initRewriteState d)
