{-# LANGUAGE TemplateHaskell #-}
module Database.Algebra.Pathfinder.Render.XML (document, mkXMLDocument, mkPlanBundle, serializeAlgebra,
                                          module Database.Algebra.Pathfinder.Render.XMLUtils,
                                          module Text.XML.HaXml.Types,
                                          iterCol, posCol, mkQueryPlan) where
{-
Transform a query plan DAG into an XML representation.
-}
import           Control.Monad.Writer hiding (All, Sum)
import           Database.Algebra.Dag.Common
import           Database.Algebra.Impossible
import           Database.Algebra.Pathfinder.Data.Algebra
import           Database.Algebra.Pathfinder.Render.XMLUtils

import           Text.PrettyPrint.HughesPJ
import           Text.XML.HaXml.Escape                       (stdXmlEscaper, xmlEscape)
import qualified Text.XML.HaXml.Pretty                       as P (document)
import           Text.XML.HaXml.Types

import           Data.List                                   (intersperse, transpose)

document ::  Document i -> Doc
document = P.document

-- Serialize algebra
serializeAlgebra :: [Element ()] -> GraphNode -> XML XMLNode
serializeAlgebra cols qGId = do
                                    qId <- alg2XML qGId
                                    nilId <- nilNode
                                    xId <- freshId
                                    let contentN = cols `childsOf` contentNode
                                    let edgeNil = mkEdge nilId
                                    let edgeQ = mkEdge qId
                                    tell [[contentN, edgeNil, edgeQ] `childsOf` node xId "serialize relation"]
                                    return xId

-- XML defintion of iter column
iterCol :: Element ()
iterCol =  [attr "name" "iter", attr "new" "false", attr "function" "iter"] `attrsOf` xmlElem "column"

-- XML defintion of position column
posCol :: Element ()
posCol = [attr "name" "pos", attr "new" "false", attr "function" "pos"] `attrsOf` xmlElem "column"

-- XML defintion of nil node
nilNode :: XML XMLNode
nilNode = do
            xId <- freshId
            tell [node xId "nil"]
            return xId

-- Transform algebra into XML
-- The outer function determines whether the node was already translated into xml, if so it returns the xml id of that node.
-- if the node was not translated yet the inner function alg2XML' will translated the plan and return the xml id
alg2XML :: GraphNode -> XML XMLNode
alg2XML gId = do
                def <- isDefined gId
                case def of
                    Just x -> return x
                    Nothing -> do
                                debug <- debugEnabled
                                nd <- getNode gId
                                xId <- alg2XML' nd
                                addNodeTrans gId xId
                                if debug
                                    then
                                      do
                                        ts <- getTags gId
                                        case ts of
                                            Nothing -> return xId
                                            Just x -> do
                                                        xId' <- alg2XML' (UnOp (Dummy (concat $ intersperse " " x)) gId)
                                                        addNodeTrans gId xId'
                                                        return xId'
                                            else
                                                return xId
 where
    alg2XML' :: PFAlgebra -> XML XMLNode
    alg2XML' (TerOp _ _ _ _) = $impossible
    alg2XML' (NullaryOp (LitTable vs s)) = do
                                            xId <- freshId
                                            tell [mkTableNode xId s vs]
                                            return xId
    alg2XML' (UnOp (Attach (n, (ty, val))) cId1) = do
                                                cxId1 <- alg2XML cId1
                                                xId <- freshId
                                                tell [mkAttachNode xId n val ty cxId1]
                                                return xId
    alg2XML' (UnOp (Proj proj) cId1) = do
                                    cxId1 <- alg2XML cId1
                                    xId <- freshId
                                    tell [mkProjNode xId proj cxId1]
                                    return xId
    alg2XML' (BinOp (EqJoin jc) cId1 cId2) = do
                                                cxId1 <- alg2XML cId1
                                                cxId2 <- alg2XML cId2
                                                xId <- freshId
                                                tell [mkEqJoinNode xId jc cxId1 cxId2]
                                                return xId
    alg2XML' (BinOp (ThetaJoin ji) cId1 cId2) = do
                                          cxId1 <- alg2XML cId1
                                          cxId2 <- alg2XML cId2
                                          xId <- freshId
                                          tell [mkThetaJoinNode xId ji cxId1 cxId2]
                                          return xId
    alg2XML' (UnOp (FunBinOp (op, res, lArg, rArg)) cId) = do
                                                        cxId1 <- alg2XML cId
                                                        xId <- freshId
                                                        tell [mkBinOpNode xId op res lArg rArg cxId1]
                                                        return xId
    alg2XML' (NullaryOp (EmptyTable schema)) = do
                                         xId <- freshId
                                         tell [mkEmptyTable xId schema]
                                         return xId
    alg2XML' (BinOp (DisjUnion _)cId1 cId2) = do
                                          cxId1 <- alg2XML cId1
                                          cxId2 <- alg2XML cId2
                                          xId <- freshId
                                          tell [mkUnion xId cxId1 cxId2]
                                          return xId
    alg2XML' (UnOp (Rank (res, sort)) cId1) = do
                                            cxId1 <- alg2XML cId1
                                            xId <- freshId
                                            tell [mkRank xId res sort cxId1]
                                            return xId
    alg2XML' (BinOp (Cross ()) cId1 cId2) = do
                                        cxId1 <- alg2XML cId1
                                        cxId2 <- alg2XML cId2
                                        xId <- freshId
                                        tell [mkCross xId cxId1 cxId2]
                                        return xId
    alg2XML' (NullaryOp (TableRef (n, cs, ks))) = do
                                            xId <- freshId
                                            tell [mkTable xId n cs ks]
                                            return xId
    alg2XML' (UnOp (Sel n) cId1) = do
                                cxId <- alg2XML cId1
                                xId <- freshId
                                tell [mkSelect xId n cxId]
                                return xId
    alg2XML' (UnOp (PosSel (n, sort, part)) cId1) = do
                                                  cxId1 <- alg2XML cId1
                                                  xId <- freshId
                                                  tell [mkPosSel xId n sort part cxId1]
                                                  return xId
    alg2XML' (UnOp (FunBoolNot (res, col)) cId1) = do
                                                 cxId1 <- alg2XML cId1
                                                 xId <- freshId
                                                 tell [mkBoolNot xId res col cxId1]
                                                 return xId
    alg2XML' (UnOp (RowNum (res, sort, part)) cId1) = do
                                                    cxId1 <- alg2XML cId1
                                                    xId <- freshId
                                                    tell [mkRowNum xId res sort part cxId1]
                                                    return xId
    alg2XML' (UnOp (Distinct ()) cId1) = do
                                    cxId <- alg2XML cId1
                                    xId <- freshId
                                    tell [mkDistinct xId cxId]
                                    return xId
    alg2XML' (UnOp (RowRank (res, sort)) cId1) = do
                                              cxId1 <- alg2XML cId1
                                              xId <- freshId
                                              tell [mkRowRank xId res sort cxId1]
                                              return xId
    alg2XML' (UnOp (Aggr (aggrs, part)) cId1)
                            = do
                                cxId1 <- alg2XML cId1
                                xId <- freshId
                                tell [mkAggrs xId aggrs part cxId1]
                                return xId
    alg2XML' (UnOp (Cast (r, o, t)) cId1) = do
                                        cxId1 <- alg2XML cId1
                                        xId <- freshId
                                        tell [mkCast xId o r t cxId1]
                                        return xId
    alg2XML' (BinOp (Difference ()) cId1 cId2) = do
                                        cxId1 <- alg2XML cId1
                                        cxId2 <- alg2XML cId2
                                        xId <- freshId
                                        tell [mkDifference xId cxId1 cxId2]
                                        return xId
    alg2XML' (UnOp (Dummy t) cId1) = do
                                cxId1 <- alg2XML cId1
                                xId <- freshId
                                tell [mkDummy xId t cxId1]
                                return xId

mkDummy :: XMLNode -> String -> XMLNode -> Element ()
mkDummy xId comment cxId1 =  ([[comment `stringChildOf` xmlElem "comment"] `childsOf` contentNode ,mkEdge cxId1]`childsOf` node xId "dummy")

mkDifference :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkDifference xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2] `childsOf` node xId "difference"

mkCast :: XMLNode -> AttrName -> AttrName -> ATy -> XMLNode -> Element ()
mkCast xId o r t c = [[column r True, column o False, typeN t] `childsOf` contentNode, mkEdge c] `childsOf` node xId "cast"

mkAggrs :: XMLNode -> [(AggrType, ResAttrName)] -> Maybe PartAttrName -> XMLNode -> Element ()
mkAggrs xId aggrs part cId = let partCol = case part of
                                            Nothing -> []
                                            Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                 aggr = map mkAggr aggrs
                              in [(partCol ++ aggr) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "aggr"
    where
    
        unaryAggr :: String -> AttrName -> ResAttrName -> Element ()
        unaryAggr kind argCol resCol =
            [resColNode, argColNode] `childsOf` ([attr "kind" kind] `attrsOf` xmlElem "aggregate")
          where 
            resColNode = column resCol True
            argColNode = [attr "function" "item"] `attrsOf` column argCol False
            
        nullaryAggr :: String -> ResAttrName -> Element ()
        nullaryAggr kind resCol =
            [resColNode] `childsOf` ([attr "kind" kind] `attrsOf` xmlElem "aggregate")
          where 
            resColNode = column resCol True
    
        mkAggr :: (AggrType, ResAttrName) -> Element ()
        mkAggr (aggr, res) =
            case aggr of
                Avg c -> unaryAggr "avg" c res
                Max c -> unaryAggr "max" c res
                Min c -> unaryAggr "min" c res
                Sum c -> unaryAggr "sum" c res
                All c -> unaryAggr "all" c res
                Prod c -> unaryAggr "prod" c res
                Dist c -> unaryAggr "distinct" c res
                Count -> nullaryAggr "count" res

mkPosSel :: XMLNode -> Int -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkPosSel xId n sort part cId = let sortCols = map mkSortColumn $ zip sort [1..]
                                   partCol = case part of
                                                   Nothing -> []
                                                   Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                   posNode = n `dataChildOf` xmlElem "position"
                                in [((posNode:sortCols) ++ partCol) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "pos_select"

-- Create an xml rank element node.
mkRowRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRowRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                              in [(column res True : sortCols) `childsOf` contentNode, mkEdge cId] `childsOf` node xId "rowrank"

-- | Create an xml distinct node
mkDistinct :: XMLNode -> XMLNode -> Element ()
mkDistinct xId cxId = [mkEdge cxId] `childsOf` node xId "distinct"

-- | Create an xml rownum node
mkRowNum :: XMLNode -> ResAttrName -> SortInf -> Maybe PartAttrName -> XMLNode -> Element ()
mkRowNum xId res sort part cxId = let sortCols = map mkSortColumn $ zip sort [1..]
                                      partCol = case part of
                                                    Nothing -> []
                                                    Just x  -> [[attr "function" "partition"] `attrsOf` column x False]
                                   in [(column res True:(sortCols ++ partCol)) `childsOf` contentNode , mkEdge cxId] `childsOf` node xId "rownum"

-- | Create an xml boolean not node
mkBoolNot :: XMLNode -> String -> String -> XMLNode -> Element ()
mkBoolNot xId res arg cxId = [[column res True, column arg False] `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "not"

-- | Create an xml select node
mkSelect :: XMLNode -> String -> XMLNode -> Element ()
mkSelect xId n cxId = [[column n False] `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "select"

-- | Create an xml table binding node
mkTable :: XMLNode -> String -> TableAttrInf -> KeyInfos -> Element ()
mkTable xId n descr keys = [[mkKeys keys] `childsOf` xmlElem "properties", [mkTableDescr n descr] `childsOf` contentNode] `childsOf` node xId "ref_tbl"

-- | Create an xml table description node
mkTableDescr :: String -> TableAttrInf -> Element ()
mkTableDescr n descr = map (\d -> toTableCol d ) descr `childsOf` [attr "name" n] `attrsOf` xmlElem "table"
    where
     toTableCol :: (AttrName, AttrName, ATy) -> Element ()
     toTableCol (cn, xn, t) = [attr "name" xn, attr "tname" cn, attr "type" $ show t] `attrsOf` xmlElem "column"

-- | Create an xml table key node
mkKey :: KeyInfo -> Element ()
mkKey k = let bd = map (\(k', p) -> [attr "name" k', attr "position" $ show p] `attrsOf` xmlElem "column") $ zip k [1..]
           in bd `childsOf` xmlElem "key"

-- | Create an xml node containing multiple table keys
mkKeys :: KeyInfos -> Element ()
mkKeys ks = map mkKey ks `childsOf` xmlElem "keys"

-- Create an xml rank element node.
mkRank :: XMLNode -> ResAttrName -> SortInf -> XMLNode -> Element ()
mkRank xId res sort cId = let sortCols = map mkSortColumn $ zip sort [1..]
                              resCol = column res True
                           in [resCol:sortCols `childsOf` contentNode, mkEdge cId] `childsOf` node xId "rank"

-- Create an xml sort column node for use in the rank node.
mkSortColumn :: ((SortAttrName, SortDir), Int) -> Element ()
mkSortColumn ((n, d), p) = [attr "function" "sort", attr "position" $ show p, attr "direction" $ show d] `attrsOf` column n False


-- Create an xml cross node
mkCross :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkCross xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "cross"

-- Create an xml union node
mkUnion :: XMLNode -> XMLNode -> XMLNode -> Element ()
mkUnion xId cxId1 cxId2 = [mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "union"

-- Create an empty table node, table needs to contain type information
mkEmptyTable :: XMLNode -> SchemaInfos -> Element ()
mkEmptyTable xId schema = [map mkColumn schema `childsOf` contentNode] `childsOf` node xId "empty_tbl"

-- Create an xml column node
mkColumn :: (AttrName, ATy) -> Element ()
mkColumn (n, t) = [attr "type" $ show t] `attrsOf` column n True

-- Create an xml binary operator node.
-- Three sort of binary operators exist:
--  1. Arithmatic operators, represented in xml as function nodes
--  2. Relational operators, represented in xml as relational function nodes
--  3. Operators that can be expressed in terms of other operators
mkBinOpNode :: XMLNode -> Fun -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkBinOpNode xId (Fun1to1 f) res lArg rArg cId = mkFnNode xId (show f) res lArg rArg cId
mkBinOpNode xId (RelFun Lt) res lArg rArg cId = mkBinOpNode xId (RelFun Gt) res rArg lArg cId
mkBinOpNode xId (RelFun r) res lArg rArg cId = mkRelFnNode xId (show r) res lArg rArg cId

-- Create an XML relational function node
mkRelFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkRelFnNode xId fn res lArg rArg cId = let content = [column res True,
                                                      [attr "position" "1"] `attrsOf` column lArg False,
                                                      [attr "position" "2"] `attrsOf` column rArg False] `childsOf` contentNode
                                        in [content, mkEdge cId] `childsOf` node xId fn


-- Create an XML function node
mkFnNode :: XMLNode -> String -> ResAttrName -> LeftAttrName -> RightAttrName -> XMLNode -> Element ()
mkFnNode xId fn res lArg rArg cId = let cont = [[attr "name" fn] `attrsOf` xmlElem "kind",
                                                column res True,
                                                [attr "position" "1"] `attrsOf` column lArg False,
                                                [attr "position" "2"] `attrsOf` column rArg False] `childsOf` contentNode
                                     in [cont, mkEdge cId] `childsOf` node xId "fun"

-- Create an XML eq-join node.
mkEqJoinNode :: XMLNode -> (LeftAttrName,RightAttrName) -> XMLNode -> XMLNode -> Element ()
mkEqJoinNode xId (lN, rN) cxId1 cxId2 = let contNode = [[attr "position" "1"] `attrsOf` column lN False,
                                                        [attr "position" "2"] `attrsOf` column rN False] `childsOf` contentNode
                                         in [contNode, mkEdge cxId1, mkEdge cxId2]`childsOf` node xId "eqjoin"

-- Create an XML theta-join node.
mkThetaJoinNode :: XMLNode -> [(LeftAttrName,RightAttrName, JoinRel)] -> XMLNode -> XMLNode -> Element ()
mkThetaJoinNode xId joinCond cxId1 cxId2 = let contNode = [ [[attr "position" "1"] `attrsOf` column lN False,
                                                                [attr "position" "2"] `attrsOf` column rN False]
                                                                   `childsOf`
                                                                     ([attr "kind" $ show o] `attrsOf` xmlElem "comparison")
                                                             | (lN, rN, o) <- joinCond
                                                             ]
                                                             `childsOf` contentNode
                                         in [contNode, mkEdge cxId1, mkEdge cxId2] `childsOf` node xId "thetajoin"

-- Create an XML projection node
mkProjNode :: XMLNode -> [(NewAttrName, OldAttrName)] -> XMLNode -> Element ()
mkProjNode xId mapping cxId = [map mkProjColumn mapping `childsOf` contentNode, mkEdge cxId] `childsOf` node xId "project"
    where
      mkProjColumn :: (NewAttrName, OldAttrName) -> Element ()
      mkProjColumn (n, o) = [attr "old_name" o] `attrsOf` column n True

-- Create an xml attach column node
mkAttachNode :: XMLNode -> ColName -> AVal -> ATy -> XMLNode -> Element ()
mkAttachNode xId n val ty cxId = let valNode = val `dataChildOf` [attr "type" $ show ty] `attrsOf` xmlElem "value"
                                     colNode = [xmlEscape stdXmlEscaper valNode] `childsOf` column n True
                                  in [[colNode] `childsOf` contentNode, mkEdge cxId]`childsOf` node xId "attach"

-- Create an xml table node
mkTableNode :: XMLNode -> [(ColName, ATy)] -> [[AVal]] -> Element ()
mkTableNode xId s vals = let colNodes = [ valNodes t vs `childsOf` column n True
                                         | ((n, t), vs) <- zip s (transpose vals)]
                             conNode  = colNodes `childsOf` contentNode
                          in [conNode] `childsOf` node xId "table"
  where
      valNodes ty vs = [xmlEscape stdXmlEscaper $ v `dataChildOf` [attr "type" $ show ty] `attrsOf` xmlElem "value" | v <- vs]

-- Create an xml edge to point to the given xml node id.
mkEdge :: XMLNode -> Element ()
mkEdge n = [attr "to" $ show n] `attrsOf` xmlElem "edge"

-- Transform the given plan nodes into an xml query plan.
-- The first argument can contain additional property node information
mkQueryPlan :: Maybe (Int, Int) -> Element () -> [Element ()] -> XML Int
mkQueryPlan parent props els = let logicalPlan = els `childsOf` [attr "unique_names" "true"] `attrsOf` xmlElem "logical_query_plan"
                         in do
                             planId <- freshId
                             let attrs = case parent of
                                            Nothing -> [attr "id" $ show planId]
                                            Just (p, c) -> [attr "id" $ show planId, attr "idref" $ show p, attr "colref" $ show c]
                             tell [[props, logicalPlan] `childsOf` attrs `attrsOf` xmlElem "query_plan"]
                             return planId

-- Create a plan bundle out of the given query plans
mkPlanBundle :: [Element ()] -> Element ()
mkPlanBundle plans = plans `childsOf` xmlElem "query_plan_bundle"

-- Create an xml document out of the given root tag.
mkXMLDocument :: Element () -> Document ()
mkXMLDocument el = let xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
                       prol = Prolog (Just xmlDecl) [] Nothing []
                    in Document prol emptyST el []
