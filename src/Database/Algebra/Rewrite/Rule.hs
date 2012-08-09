module Database.Algebra.Rewrite.Rule
       ( Rule
       , RuleSet
       , applyRuleSet ) where

import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Match

type Rule r o p = AlgNode -> Match o p (r o ())
              
type RuleSet r o p = [Rule r o p]

-- | Try a set of rules on a node and apply the rewrite of the first
-- rule that matches.
applyRuleSet :: DagRewrite (r o) o => NodeMap p -> RuleSet r o p -> AlgNode -> r o Bool
applyRuleSet pm rules q = do
  d <- getDag
  
  let aux []        = return False
      aux (rule:rs) = case runMatch d pm (rule q) of
                          Just rewrite -> rewrite >> return True
                          Nothing      -> aux rs
      
  aux rules
  
