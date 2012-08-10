module Database.Algebra.Rewrite.Traversal
       ( preOrder
       , postOrder
       , topologically
       , iteratively
       , sequenceRewrites ) where

import Control.Monad
  
import qualified Data.Set as S

import qualified Database.Algebra.Dag as Dag
import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Rule

-- | Infer properties, then traverse the DAG in preorder fashion and apply the rule set 
-- at every node. Properties are re-inferred after every change.
preOrder :: ( DagRewrite (r o) o
            , Dag.Operator o)
            => (Dag.AlgebraDag o -> NodeMap p -> m o p (r o ()) -> Maybe (r o ()))
            -> r o (NodeMap p) 
            -> RuleSet m r o p 
            -> r o Bool
preOrder applyMatch inferAction rules = 
  let traverse (changedPrev, mProps, visited) q =
        if q `S.member` visited
        then return (changedPrev, mProps, visited)
        else do
          props <- case mProps of
            Just ps -> return ps
            Nothing -> inferAction

          changedSelf <- applyRuleSet applyMatch props rules q
      
          let mProps' = if changedSelf then Nothing else Just props
          op <- operator q
          let cs = Dag.opChildren op
          (changedChild, mProps'', visited') <- foldM descend (changedSelf, mProps', visited) cs
          let visited'' = S.insert q visited'
          if changedChild 
            then return (True, Nothing, visited'')
            else return (changedPrev || (changedSelf || changedChild), mProps'', visited'')

      descend (changedPrev, mProps, visited) c = do
          props <- case mProps of
            Just ps -> return ps
            Nothing -> inferAction
          traverse (changedPrev, Just props, visited) c

  in do
    pm <- inferAction
    rs <- rootNodes
    (changed, _, _) <- foldM traverse (False, Just pm, S.empty) rs
    return changed

{- | Map a ruleset over the nodes of a DAG in topological order. This function assumes that
     the structur of the DAG is not changed during the rewrites. Properties are only inferred
     once.
-}
topologically :: ( DagRewrite (r o) o
                 , Dag.Operator o)
                 => (Dag.AlgebraDag o -> NodeMap p -> m o p (r o ()) -> Maybe (r o ()))
                 -> r o (NodeMap p) 
                 -> RuleSet m r o p 
                 -> r o Bool
topologically applyRule inferAction rules = do
  topoOrdering <- topsort
  props <- inferAction
  let rewriteNode changedPrev q = do
        changed <- applyRuleSet applyRule props rules q
        return $ changed || changedPrev
  foldM rewriteNode False topoOrdering where

-- | Infer properties, then traverse the DAG in a postorder fashion and apply the rule set at
-- every node. Properties are re-inferred after every change.
postOrder :: ( DagRewrite (r o) o
             , Dag.Operator o)
             => (Dag.AlgebraDag o -> NodeMap p -> m o p (r o ()) -> Maybe (r o ()))
             -> r o (NodeMap p) 
             -> RuleSet m r o p 
             -> r o Bool
postOrder applyRule inferAction rules = 
  let traverse (changedPrev, props, visited) q =
        if q `S.member` visited
        then return (changedPrev, props, visited)
        else do
          op <- operator q
          let cs = Dag.opChildren op
          (changedChild, mProps, visited') <- foldM descend (False, props, visited) cs
          props' <- case mProps of
            Just ps -> return ps
            Nothing -> inferAction
        
          changedSelf <- applyRuleSet applyRule props' rules q
          let visited'' = S.insert q visited'
          if changedSelf
            then return (True, Nothing, visited'')
            else return (changedChild || changedPrev, Just props', visited'')
      
      descend (changedPrev, mProps, visited) c = do
          props <- case mProps of
            Just ps -> return ps
            Nothing -> inferAction
          traverse (changedPrev, Just props, visited) c
        
  in do
    pm <- inferAction
    rs <- rootNodes
    (changed, _, _) <- foldM traverse (False, Just pm, S.empty) rs
    return changed
  
-- | Iteratively apply a rewrite, until no further changes occur.
iteratively :: DagRewrite (r o) o => r o Bool -> r o Bool
iteratively rewrite = aux False
  where aux b = do
          changed <- rewrite
          if changed
            then logGeneral ">>> Iterate" >> aux True
            else return b
  
-- | Sequence a list of rewrites and propagate information about
-- wether one of them applied.
sequenceRewrites :: DagRewrite (r o) o => [r o Bool] -> r o Bool
sequenceRewrites rewrites = do
  changed <- sequence rewrites
  return $ or changed
  
