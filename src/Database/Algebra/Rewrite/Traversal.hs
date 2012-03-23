module Database.Algebra.Rewrite.Traversal
       ( preOrder
       , postOrder
       , iteratively ) where

import Database.Algebra.Dag
import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Rule

-- | Infer properties, then traverse the DAG in preorder fashion and apply the rule set 
-- at every node.
preOrder :: Operator o => DagRewrite o (NodeMap p) -> RuleSet o p -> DagRewrite o Bool
preOrder infer rules = 
  let traverse props q = do
        changedSelf <- applyRuleSet props rules q
        op <- operatorM q
        let cs = opChildren op
        changedChild <- mapM (\c -> traverse props c) cs
        return $ changedSelf || (or changedChild)
  in do
    pm <- infer
    rs <- rootNodesM
    changed <- mapM (\r -> traverse pm r) rs
    return $ or changed
           
-- | Infer properties, then traverse the DAG in a postorder fashion and apply the rule set at
-- every node.
postOrder :: Operator o => DagRewrite o (NodeMap p) -> RuleSet o p -> DagRewrite o Bool
postOrder infer rules = 
  let traverse props q = do
        op <- operatorM q
        let cs = opChildren op
        changedChild <- mapM (\c -> traverse props c) cs
        changedSelf <- applyRuleSet props rules q
        return $ changedSelf || (or changedChild)
  in do
    pm <- infer
    rs <- rootNodesM
    changed <- mapM (\r -> traverse pm r) rs
    return $ or changed
  
-- | Iteratively apply a rewrite, until no further changes occur.
iteratively :: DagRewrite o Bool -> DagRewrite o Bool
iteratively rewrite = aux False
  where aux b = do
          changed <- rewrite
          if changed
            then logGeneralM ">>> Iterate" >> aux True
            else return b
  
  
