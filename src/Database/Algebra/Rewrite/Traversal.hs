module Database.Algebra.Rewrite.Traversal
       ( preOrder
       , postOrder
       , topologically
       , iteratively
       , sequenceRewrites ) where

import Control.Monad

import Database.Algebra.Dag
import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Rule

-- | Infer properties, then traverse the DAG in preorder fashion and apply the rule set 
-- at every node. Properties are re-inferred after every change.
preOrder :: Operator o => DagRewrite o (NodeMap p) -> RuleSet o p -> DagRewrite o Bool
preOrder infer rules = 
  let traverse (changedPrev, mProps) q = do
        props <- case mProps of
          Just ps -> return ps
          Nothing -> infer

        changedSelf <- applyRuleSet props rules q
      
        let mProps' = if changedSelf then Nothing else Just props
        op <- operatorM q
        let cs = opChildren op
        (changedChild, mProps'') <- foldM descend (changedSelf, mProps') cs
        if changedChild 
           then return (True, Nothing)
           else return (changedPrev || (changedSelf || changedChild), mProps'')

      descend (changedPrev, mProps) c = do
          props <- case mProps of
            Just ps -> return ps
            Nothing -> infer
          traverse (changedPrev, Just props) c

  in do
    pm <- infer
    rs <- rootNodesM
    (changed, _) <- foldM traverse (False, Just pm) rs
    return changed

{- | Map a ruleset over the nodes of a DAG in topological order. This function assumes that
     the structur of the DAG is not changed during the rewrites. Properties are only inferred
     once.
-}
topologically :: Operator o => DagRewrite o (NodeMap p) -> RuleSet o p -> DagRewrite o Bool
topologically infer rules = do
  topoOrdering <- topsortM
  props <- infer
  let rewriteNode changedPrev q = do
        changed <- applyRuleSet props rules q
        return $ changed || changedPrev
  foldM rewriteNode False topoOrdering where

-- | Infer properties, then traverse the DAG in a postorder fashion and apply the rule set at
-- every node. Properties are re-inferred after every change.
postOrder :: Operator o => DagRewrite o (NodeMap p) -> RuleSet o p -> DagRewrite o Bool
postOrder infer rules = 
  let traverse (changedPrev, props) q = do
        op <- operatorM q
        let cs = opChildren op
        (changedChild, mProps) <- foldM descend (False, props) cs
        props' <- case mProps of
              Just ps -> return ps
              Nothing -> infer
        
        changedSelf <- applyRuleSet props' rules q
        if changedSelf
          then return (True, Nothing)
          else return (changedChild || changedPrev, Just props')
      
      descend (changedPrev, mProps) c = do
          props <- case mProps of
            Just ps -> return ps
            Nothing -> infer
          traverse (changedPrev, Just props) c
        
  in do
    pm <- infer
    rs <- rootNodesM
    (changed, _) <- foldM traverse (False, Just pm) rs
    return changed
  
-- | Iteratively apply a rewrite, until no further changes occur.
iteratively :: DagRewrite o Bool -> DagRewrite o Bool
iteratively rewrite = aux False
  where aux b = do
          changed <- rewrite
          if changed
            then logGeneralM ">>> Iterate" >> aux True
            else return b
  
-- | Sequence a list of rewrites and propagate information about
-- wether one of them applied.
sequenceRewrites :: [DagRewrite o Bool] -> DagRewrite o Bool
sequenceRewrites rewrites = do
  changed <- sequence rewrites
  return $ or changed
  
