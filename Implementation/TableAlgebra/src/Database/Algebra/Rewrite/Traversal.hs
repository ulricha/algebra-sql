module Database.Algebra.Rewrite.Traversal
       ( preOrder
       , postOrder
       , iteratively ) where

import Database.Algebra.Dag
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Rule

preOrder :: Operator o => RuleSet o -> DagRewrite o Bool
preOrder rules = 
  let traverse q = do
        changedSelf <- applyRuleSet q rules
        op <- operatorM q
        let cs = opChildren op
        changedChild <- mapM (\c -> applyRuleSet c rules) cs
        return $ changedSelf || (or changedChild)
  in do
    rs <- rootNodesM
    changed <- mapM (\r -> traverse r) rs
    return $ or changed
           
postOrder :: Operator o => RuleSet o -> DagRewrite o Bool
postOrder rules = 
  let traverse q = do
        op <- operatorM q
        let cs = opChildren op
        changedChild <- mapM (\c -> applyRuleSet c rules) cs
        changedSelf <- applyRuleSet q rules
        return $ changedSelf || (or changedChild)
  in do
    rs <- rootNodesM
    changed <- mapM (\r -> traverse r) rs
    return $ or changed
  
iteratively :: DagRewrite a Bool -> DagRewrite a ()
iteratively rewrite = do
  changed <- rewrite
  if changed
    then iteratively rewrite
    else return ()
  
  
