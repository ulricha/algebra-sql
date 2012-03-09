Types for peephole style optimizations on general algebra DAGs.

So far, a "ruleset" consisted of a number of case clauses in a single function, which
were applied to an operator. This should be quite efficient. 

The new approach wraps every single pattern in a separate function. Applying a number
of rules means that a list of rules has to be traversed and (potentially) every
function/monadic action in it has to be applied. This could be quite costly in comparion.
Does the syntactic sugar outweigh the performance disadvantage?

> type DagNode

The DAG store does not need to be a monad at all. It is just an (abstract) type
that contains the current state of the DAG. This is the state of the Rewrite monad
(plus a potential cache). This state is exposed to perform a match on the DAG.
For the match, this state is wrapped read-only into the Match monad.

Access to the content of the state container is hidden in the Match and Rewrite monads.
       
> type DagStore = DagState
                   
> type Rewrite = undefined

The rewrite monad provides read-only access to the operators and certain topological
properties of the DAG. It includes the possibility of failure. A Match action might
fail, which leads to the failure of the complete match. In this case, no result
is returned.
                 
Failure is modelled by including MaybeT in the monad stack.

Only read access to the DAG is needed. Therefore, a Reader monad would be sufficient
(and maybe more efficient than State).

The Match monad needs logging capabilities, to include logs when the match fails
and no rewrite is performed.
                
> type Match
  
runMatch receives the current State of the DAG and then performs a Match with this
state/DAG. It returns either a rewrite action if the match succeeds or nothing.

> runMatch :: AlgNode -> Match Rewrite -> DagState -> Maybe (Rewrite ())

Runs a function (a "pattern") on the operator that belongs to a certain node
              
Is the Operator requirement really necessary?                            

> matchOp :: Operator o => DagNode -> (o -> Match a) -> Match a
             
> predicate :: Bool -> Maybe ()                          
           
Returns the parents of a node. This is e.g. necessary to decide if a node
has only one parent (see projection merging and selection pushdown in the
X100 rewrites).

> parents :: Operator o => DagNode -> Match [DagNode]

A rewrite rule consists of a Match, that -- if it succeeds -- returns a 
Rewrite action.

> type Rule = Match (Rewrite ())

> type RuleSet
              
Apply a rule. If the match succeeds, perform the accompanying Rewrite action. If not, do 
nothing

> applyRule :: DagState -> AlgNode -> Rule -> Rewrite ()

A ruleset needs to be executed: Every Rule in the set needs to be matched against
the given node. If the match is successfull, the returned rewrite action should
be executed. If the match is not successfull, the next rule should be tried on the
same node. 
                 
> applyRuleSet :: DagState -> AlgNode -> RuleSet -> Rewrite ()
