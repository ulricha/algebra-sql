module Database.Algebra.Rewrite.Rule
       ( Rule
       , RuleSet
       , applyRuleSet ) where

import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Match

type Rule o p = AlgNode -> Match o p (DagRewrite o ())
              
type RuleSet o p = [Rule o p]

-- | Try a set of rules on a node and apply the rewrite of the first
-- rule that matches.
applyRuleSet :: NodeMap p -> RuleSet o p -> AlgNode -> DagRewrite o Bool
applyRuleSet pm rules q = do
  d <- dagM
  
  let aux []     = return False
      aux (rule:rs) = case runMatch d pm (rule q) of
                          Just rewrite -> rewrite >> return True
                          Nothing      -> aux rs
      
  aux rules
  
