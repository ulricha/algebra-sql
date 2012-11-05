module Database.Algebra.Rewrite.Rule
       ( Rule
       , RuleSet
       , applyRuleSet ) where

import Database.Algebra.Dag.Common
import Database.Algebra.Rewrite.DagRewrite
import Database.Algebra.Rewrite.Match

type Rule o p e = AlgNode -> Match o p e (Rewrite o e ())
              
type RuleSet o p e = [Rule o p e]

-- | Try a set of rules on a node and apply the rewrite of the first
-- rule that matches.
applyRuleSet :: e -> NodeMap p -> RuleSet o p e -> AlgNode -> Rewrite o e Bool
applyRuleSet e pm rules q = do
  d <- exposeDag
  
  let aux []        = return False
      aux (rule:rs) = case runMatch e d pm (rule q) of
                          Just rewrite -> rewrite >> return True
                          Nothing      -> aux rs
      
  aux rules
  
