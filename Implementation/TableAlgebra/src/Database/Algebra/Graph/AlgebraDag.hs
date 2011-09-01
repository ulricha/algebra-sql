module Database.Algebra.Graph.AlgebraDag(AlgebraDag,
                                         mkDag,
                                         nodeMap,
                                         Operator,
                                         opChildren,
                                         replaceOpChild,
                                         insert,
                                         replace,
                                         delete,
                                         parents,
                                         replaceChild,
                                         topsort,
                                         operator,
                                         RewriteState,
                                         dag,
                                         DagRewrite,
                                         rewriteState,
                                         insertM,
                                         replaceM,
                                         deleteM,
                                         parentsM,
                                         replaceChildM,
                                         topsortM,
                                         operatorM,
                                         pruneUnusedM)
       where

import Database.Algebra.Graph.Common 

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Graph.Inductive.PatriciaTree

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State

data AlgebraDag a = AlgebraDag {
    nodeMap :: NodeMap a,
    graph :: UGr
}

class Operator a where
    opChildren :: a -> [AlgNode]
    replaceOpChild :: a -> AlgNode -> AlgNode -> a

mkDag :: Operator a => NodeMap a -> AlgebraDag a
mkDag m = AlgebraDag { nodeMap = m, graph = g }
    where g = uncurry G.mkUGraph $ M.foldrWithKey aux ([], []) m
          aux n op (allNodes, allEdges) = (n : allNodes, es ++ allEdges)
              where es = map (\v -> (n, v)) $ opChildren op

insert :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
insert n op d = 
    let cs = opChildren op
        g' = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ graph d
        m' = M.insert n op $ nodeMap d
    in AlgebraDag { nodeMap = m', graph = g' }

replace :: Operator a => AlgNode -> a -> AlgebraDag a -> AlgebraDag a
replace n op d = 
    let cs = opChildren op
        g' = G.insEdges (map (\c -> (n, c, ())) cs) $ G.insNode (n, ()) $ G.delNode n $ graph d
        m' = M.insert n op $ nodeMap d
    in AlgebraDag { nodeMap = m', graph = g' }

delete :: Operator a => AlgNode -> AlgebraDag a -> AlgebraDag a
delete n d =
    let g' = G.delNode n $ graph d
        m' = M.delete n $ nodeMap d
    in AlgebraDag { nodeMap = m', graph = g' }

parents :: AlgNode -> AlgebraDag a -> [AlgNode]
parents n d = G.pre (graph d) n

replaceChild :: Operator a => AlgNode -> AlgNode -> AlgNode -> AlgebraDag a -> AlgebraDag a
replaceChild n old new d = replace n (replaceOpChild (operator n d) old new) d

topsort :: Operator a => AlgebraDag a -> [AlgNode]
topsort d = DFS.topsort $ graph d

operator :: AlgNode -> AlgebraDag a -> a
operator n d = 
    case M.lookup n $ nodeMap d of
        Just op -> op
        Nothing -> error $ "AlgebraDag.operator: lookup failed for " ++ (show n)

data RewriteState a = RewriteState {
    supply :: AlgNode,
    dag :: AlgebraDag a
    }

rewriteState :: AlgebraDag a -> RewriteState a
rewriteState d =
    let maxID = fst $ M.findMax $ nodeMap d
    in RewriteState { supply = maxID + 1, dag = d }

freshNodeID :: DagRewrite a AlgNode
freshNodeID =
    do
        s <- get
        let n = supply s
        put $ s { supply = n + 1 }
        return n

type DagRewrite a b = State (RewriteState a) b

insertM :: Operator a => a -> DagRewrite a ()
insertM op = 
    do
        n <- freshNodeID
        s <- get
        put $ s { dag = insert n op $ dag s }

replaceM :: Operator a => AlgNode -> a -> DagRewrite a ()
replaceM n op = 
    do
        s <- get
        put $ s { dag = replace n op $ dag s }

deleteM :: Operator a => AlgNode -> DagRewrite a ()
deleteM n = 
    do
        s <- get
        put $ s { dag = delete n $ dag s }

parentsM :: AlgNode -> DagRewrite a [AlgNode]
parentsM n = 
    do
        d <- gets dag
        return $ parents n d

replaceChildM :: Operator a => AlgNode -> AlgNode -> AlgNode -> DagRewrite a ()
replaceChildM n old new = 
    do
        s <- get
        put $ s { dag = replaceChild n old new $ dag s }


topsortM :: Operator a => DagRewrite a [AlgNode]
topsortM = 
    do
        d <- gets dag
        return $ topsort d

operatorM :: AlgNode -> DagRewrite a a
operatorM n = 
    do
        d <- gets dag
        return $ operator n d

{-
map :: (a -> b) -> AlgebraDag a -> AlgebraDag b
map = undefined
-}

pruneUnusedM :: [AlgNode] -> DagRewrite a ()
pruneUnusedM roots =
    do
        s <- get
        let g = graph $ dag s
            m = nodeMap $ dag s
            allNodes = S.fromList $ G.nodes g
            reachableNodes = S.fromList $ concat $ map (flip DFS.reachable g) roots
            unreachableNodes = S.difference allNodes reachableNodes
            g' = G.delNodes (S.toList $ unreachableNodes) g
            m' = foldr M.delete m $ S.toList unreachableNodes
        put $ s { dag = AlgebraDag { nodeMap = m', graph = g' } }
         
