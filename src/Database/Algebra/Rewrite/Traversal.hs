module Database.Algebra.Rewrite.Traversal
       ( preOrder
       , postOrder
       , iteratively ) where

import Control.Monad

import Database.Algebra.Dag
import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Rule

-- | Infer properties, then traverse the DAG in preorder fashion and apply the rule set 
-- at every node.
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
           
-- | Infer properties, then traverse the DAG in a postorder fashion and apply the rule set at
-- every node.
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
  
  
