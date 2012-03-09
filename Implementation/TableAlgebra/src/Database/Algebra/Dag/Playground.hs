type Rule a = Match a (DagRewrite a ())
              
match :: Operator a => AlgNode -> (a -> Match a b) -> Match a b
match = undefined
        
predicate :: Bool -> Match a ()
predicate = undefined
            
constAggrPattern :: AlgNode -> Rule X100Algebra
constAggrPattern n = do
  match n (\op -> case op of 
              UnOp (Aggr (keys, [])) c -> return (c, keys) 
              _ -> fail )
  predicate $ not $ M.null constCols
  let action = do
        logRewrite "foobar"
        replaceM $ n 
  return action
  
pushMergeRule :: AlgNode -> Rule X100Algebra
pushMergeRule q = do
  ((q1, q2), sem) <- match q [|p| BinOp (MergeJoin1 sem) q1 q2 -> return ((q1, q2), sem) |]
  (c, ps) <- match q1 [|p| UnOp (Project ps) c -> return (c, q3) |]
  predicate $ b == joinCol
  return $ do
    logRewrite "Pushdown.MergeJoin1"
    joinNode' <- insertM $ BinOp (MergeJoin1 sem) q3 q2
    projectNode' <- insertM $ UnOp (Project ps) joinNode'
    relinkParentsM q projectNode'

pushMergeRule :: AlgNode -> Rule X100Algebra
pushMergeRule q = do
  ((q1, q2), sem) <- match q [|p| MergeJoin1 sem q1 q2 -> return ((q1, q2), sem) |]
  (c, ps) <- match q1 [|p| Project ps c -> return (c, q3) |]
  predicate $ b == joinCol
  return $ do
    logRewrite "Pushdown.MergeJoin1"
    joinNode' <- insertM $ BinOp (MergeJoin1 sem) q3 q2
    projectNode' <- insertM $ UnOp (Project ps) joinNode'
    relinkParentsM q projectNode'
