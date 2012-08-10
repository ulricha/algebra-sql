module Database.Algebra.Rewrite.Rule
       ( Rule
       , RuleSet
       , applyRuleSet ) where

import Database.Algebra.Dag.Common
import Database.Algebra.Dag
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Match

type Rule m r o p = AlgNode -> m o p (r o ())
              
type RuleSet m r o p = [Rule m r o p]

-- | Try a set of rules on a node and apply the rewrite of the first
-- rule that matches.
applyRuleSet :: ( DagRewrite (r o) o
                , DagMatch (m o p) o p) 
                => (AlgebraDag o -> NodeMap p -> m o p (r o ()) -> Maybe (r o ()))
                -> NodeMap p 
                -> RuleSet m r o p 
                -> AlgNode 
                -> r o Bool
applyRuleSet applyMatch pm rules q = do
  d <- getDag
  
  let aux []        = return False
      aux (rule:rs) = case applyMatch d pm (rule q) of
                          Just rewrite -> rewrite >> return True
                          Nothing      -> aux rs
      
  aux rules
  
