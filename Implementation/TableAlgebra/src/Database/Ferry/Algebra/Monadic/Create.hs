-- | This module exports monadic combinators for creating graphs
module Database.Ferry.Algebra.Monadic.Create (attachM, castM, eqJoinM, eqTJoinM, rankM, differenceM, rowrankM, posSelectM, selectM,
                                              distinctM, crossM, notM, unionM, projM, aggrM, rownumM, rownum'M, operM, tagM) where

import qualified Database.Ferry.Algebra.Data.Create as C    
import Database.Ferry.Algebra.Data.Algebra
import Database.Ferry.Algebra.Data.GraphBuilder

bind1 :: Monad m => (a -> m b) -> m a -> m b
bind1 = (=<<)
             
bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do
                a' <- a
                b' <- b
                f a' b'
                
-- | Attach a column 'ResAttrName' of type `ATy' with value
-- `AVal' in all rows to table `AlgNode'
attachM :: ResAttrName -> ATy -> AVal -> GraphM AlgNode -> GraphM AlgNode
attachM n t v = bind1 (C.attach n t v)

-- | Cast column `AttrName' to type `ATy' and give it the name 
--  `ResAttrName' afterwards.
castM :: AttrName -> ResAttrName -> ATy -> GraphM AlgNode -> GraphM AlgNode
castM n r t = bind1 (C.cast n r t)

-- | Join two plans where the columns n1 of table 1 and columns n2 of table
--  2 are equal.
eqJoinM :: String -> String -> GraphM AlgNode -> GraphM AlgNode -> GraphM AlgNode
eqJoinM n1 n2 = bind2 (C.eqJoin n1 n2)
                      
-- | The same as eqJoin but with multiple columns.
eqTJoinM :: [(String, String)] -> ProjInf -> GraphM AlgNode -> GraphM AlgNode -> GraphM AlgNode
eqTJoinM eqs projI = bind2 (C.eqTJoin eqs projI)

-- | Assign a number to each row in column 'ResAttrName' incrementing
-- sorted by `SortInf'. The numbering is not dense!
rankM :: ResAttrName -> SortInf -> GraphM AlgNode -> GraphM AlgNode
rankM res sort = bind1 (C.rank res sort)

-- | Compute the difference between two plans.
differenceM :: GraphM AlgNode -> GraphM AlgNode -> GraphM AlgNode
differenceM = bind2 C.difference

-- | Same as rank but provides a dense numbering.
rowrankM :: ResAttrName -> SortInf -> GraphM AlgNode -> GraphM AlgNode
rowrankM res sort = bind1 (C.rowrank res sort)
                        
-- | Get's the nth element(s) of a (partitioned) table.
posSelectM :: Int -> SortInf -> Maybe AttrName -> GraphM AlgNode -> GraphM AlgNode
posSelectM n sort part = bind1 (C.posSelect n sort part)
                             
-- | Select rows where the column `SelAttrName' contains True.
selectM :: SelAttrName -> GraphM AlgNode -> GraphM AlgNode
selectM sel = bind1 (C.select sel) 

-- | Remove duplicate rows
distinctM :: GraphM AlgNode -> GraphM AlgNode
distinctM = bind1 C.distinct 

-- | Make cross product from two plans
crossM :: GraphM AlgNode -> GraphM AlgNode -> GraphM AlgNode
crossM = bind2 C.cross

-- | Negate the boolen value in column n and store it in column r
notM :: AttrName -> AttrName -> GraphM AlgNode -> GraphM AlgNode
notM r n = bind1 (C.notC r n) 

-- | Union between two plans
unionM :: GraphM AlgNode -> GraphM AlgNode -> GraphM AlgNode
unionM = bind2 C.union 

-- | Project/rename certain column out of a plan
projM :: ProjInf -> GraphM AlgNode -> GraphM AlgNode
projM cols = bind1 (C.proj cols) 

-- | Apply aggregate functions to a plan
aggrM :: [(AggrType, ResAttrName, Maybe AttrName)] -> Maybe PartAttrName -> GraphM AlgNode -> GraphM AlgNode
aggrM aggrs part = bind1 (C.aggr aggrs part) 

-- | Similar to rowrank but this will assign a \emph{unique} number to every row
-- (even if two rows are equal)
rownumM :: AttrName -> [AttrName] -> Maybe AttrName -> GraphM AlgNode -> GraphM AlgNode
rownumM res sort part = bind1 (C.rownum res sort part) 

-- | Same as rownum but columns can be assigned an ordering direction
rownum'M :: AttrName -> [(AttrName, SortDir)] -> Maybe AttrName -> GraphM AlgNode -> GraphM AlgNode
rownum'M res sort part = bind1 (C.rownum' res sort part) 

-- | Apply an operator to the element in `LeftAttrName' and `RightAttrName',
-- store the result in `ResAttrName'
operM :: String -> ResAttrName -> LeftAttrName -> RightAttrName -> GraphM AlgNode -> GraphM AlgNode
operM o r la ra = bind1 (C.oper o r la ra) 

-- | Tag a subtree with a comment
tagM :: String -> GraphM AlgNode -> GraphM AlgNode
tagM s = bind1 (C.tag s)