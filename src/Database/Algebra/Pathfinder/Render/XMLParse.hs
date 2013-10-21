

module Database.Algebra.Pathfinder.Render.XMLParse
    ( deserializeQueryPlan
    ) where

import Control.Monad (liftM2)
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.IntMap (fromList)
import Data.Maybe (listToMaybe)

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Posn (noPos, Posn)
import Text.XML.HaXml.Types ( Element (Elem)
                            , Document (Document)
                            , Content (CElem, CString)
                            , QName (N)
                            , AttValue (AttValue)
                            )
import Text.XML.HaXml.Namespaces (localName) -- we don't use xml namespaces
import Text.XML.HaXml.Combinators ( CFilter
                                  , (/>)
                                  , attr
                                  , attrval
                                  , children
                                  , childrenBy
                                  , path
                                  , tag
                                  , txt
                                  , with
                                  , without
                                  )
import Text.XML.HaXml.Verbatim (verbatim)

import Database.Algebra.Dag (mkDag, AlgebraDag)
import Database.Algebra.Dag.Common (AlgNode, Algebra(NullaryOp, UnOp, BinOp))
import qualified Database.Algebra.Pathfinder.Data.Algebra as A

-- TODO:
-- handle escaped sequences and xml entities? how does verbatim work here?
-- make comments more precise about what is meant by 'node'
-- use xmlUnEscape? (see XML.hs for locations)
-- change error reporting from Maybe to (Either String) or similar

-- | Tries to deserialize a query plan. Can fail due to structural errors.
deserializeQueryPlan :: String -- ^ Filename used for error reporting.
                     -> String -- ^ Content of the file.
                     -> Maybe (AlgebraDag A.PFAlgebra)
deserializeQueryPlan filename content = do

    tuples <- mapM deserializeNode nodes
    
    let nodeIds = map fst tuples
    
    case nodeIds of
        
        l@(_:_) -> -- use the node with the smallest node id as root
                   -- TODO correct?
                   return $ mkDag (fromList tuples)
                                  [foldl min (head nodeIds) (tail nodeIds)]
        _       -> Nothing

  where (Document _ _ root _) = xmlParse filename content
        nodes                 = tag "query_plan_bundle" /> tag "query_plan"
                                /> tag "logical_query_plan"
                                /> tag "node" $ CElem root noPos


-- | Generate a node from an xml element.
deserializeNode :: Content i -> Maybe (AlgNode, A.PFAlgebra)
deserializeNode node@(CElem (Elem _ attributes contents) _) = do

    identifier <- lookupConvert readMaybe "id" attributes
    kind <- lookupVerbatim "kind" attributes

    result <- case kind of
        -- nullary operators
        "empty_tbl"  -> deserializeEmptyTable node
        "table"      -> deserializeLitTable node
        "ref_tbl"    -> deserializeTableRef node
        
        -- unary operators
        "rownum"     -> deserializeRowNum node
        "rowrank"    -> deserializeRankOperator node A.RowRank
        "rank"       -> deserializeRankOperator node A.Rank
        "project"    -> deserializeProj node
        "select"     -> deserializeSel node
        "pos_sel"    -> deserializePosSel node
        "distinct"   -> deserializeDistinct node
        "attach"     -> deserializeAttach node
        "fun"        -> deserializeBinOpFun node
        
        -- unary operators with RelFun
        "gt"         -> deserializeBinOpRelFun node A.Gt
        "lt"         -> deserializeBinOpRelFun node A.Lt
        "eq"         -> deserializeBinOpRelFun node A.Eq
        "and"        -> deserializeBinOpRelFun node A.And
        "or"         -> deserializeBinOpRelFun node A.Or
        
        -- unary operators continued
        "cast"       -> deserializeCast node
        "not"        -> deserializeFunBoolNot node
        "aggr"       -> deserializeAggr node
        "dummy"      -> deserializeDummy node
        
        -- binary operators
        "cross"      -> deserializeCross node
        "eqjoin"     -> deserializeEqJoin node
        "thetajoin"  -> deserializeThetaJoin node
        "union"      -> deserializeDisjUnion node
        "difference" -> deserializeDifference node

        _  -> Nothing

    return (identifier, result)

-- | Queries multiple attributes from a given node.
getNodeAttributes :: [String] -> Content i -> Maybe [String]
getNodeAttributes attList (CElem (Elem _ attributes _) _) =
    mapM (\att -> lookupVerbatim att attributes)
         attList

getNodeAttributes _ _ = Nothing

-- | Queries all given attributes of the given node
-- and concatenates the text node's content in front.
getNodeAttributesWithText :: [String] -> Content i -> Maybe [String]
getNodeAttributesWithText attList c = do
    queriedAttributes <- getNodeAttributes attList c
    text <- getTextChild c
    return $ text : queriedAttributes

-- | Queries the text content of a given node.
getTextChild :: Content i -> Maybe String
getTextChild c = do
    (CString _ charData _) <- listToMaybe $ childrenBy txt c
    return charData
    
-- | Queries one attribute from a given node.
getNodeAttribute :: String -> Content i -> Maybe String
getNodeAttribute attName (CElem (Elem _ attributes _) _) =
    lookupVerbatim attName attributes

getNodeAttribute _ _                                     = Nothing

-- | Assume the current node has only one child with the given tag name
-- and try to return it.
getSingletonChildByTag :: String -> Content i -> Maybe (Content i)
getSingletonChildByTag tagName = listToMaybe . (childrenBy $ tag tagName)

-- | Same as 'getSingletonChildByTag' but with a filter.
getSingletonChildByFilter :: CFilter i -> Content i -> Maybe (Content i)
getSingletonChildByFilter f  c = listToMaybe $ childrenBy f c

-- | Looks up an attribute and returns it as a 'String'.
lookupVerbatim :: String -> [(QName, AttValue)] -> Maybe String
lookupVerbatim = lookupConvert return

-- | Looks up an attribute and converts the result with the provided function.
lookupConvert :: (String -> Maybe a)  -- ^ The function to convert with
              -> String               -- ^ Name of the attribute
              -> [(QName, AttValue)]  -- ^ The attribute mapping
              -> Maybe a
lookupConvert fun name attributes = do
    attValStr <- lookup (N name) attributes
    fun $ verbatim attValStr

-- | Tries to get the name of the result attribute from within a content node.
deserializeResultAttrName :: Content i -> Maybe String
deserializeResultAttrName contentNode = do
    -- <column name=... />
    ranNode <- listToMaybe $ childrenBy ranFilter contentNode
    getNodeAttribute "name" ranNode
  where ranFilter = tag "column" `without` attr "function"

-- | Tries to get the partition attribute name from within a content node.
deserializePartAttrName :: Content i -> Maybe String
deserializePartAttrName contentNode = do
    columnNode <- listToMaybe $ childrenBy panFilter contentNode
    
    getNodeAttribute "name" columnNode
  where panFilter = tag "column"
                    `with` attrval (N "function", AttValue $ [Left "partition"])

-- | Tries to get the sort information from the content node.
deserializeSortInf :: Content i -> Maybe A.SortInf
deserializeSortInf contentNode = do
    -- <column function="sort" position=... direction=... name=... />
    let sortInfoNodes = childrenBy siFilter contentNode
    
    queriedSortInfo <- mapM (getNodeAttributes [ "position"
                                               , "name"
                                               , "direction"
                                               ]
                            )
                            sortInfoNodes

    let tupleConv :: [String] -> Maybe (A.SortAttrName, A.SortDir)
        tupleConv [_, name, dirStr] = do
            direction <- deserializeSortDir dirStr
            return (name, direction)    
        tupleConv _                 = Nothing

    mapM tupleConv $ sortBy (on compare head) queriedSortInfo
  where siFilter  = tag "column"
                    `with` attrval (N "function", AttValue $ [Left "sort"])


-- | Try to get a single child id from the edge node's to attribute.
deserializeChildId1 :: Content i -> Maybe AlgNode
deserializeChildId1 node = do
    edgeNode <- getSingletonChildByTag "edge" node
    toEdge <- getNodeAttribute "to" edgeNode
    readMaybe toEdge

-- | Try to get two child ids from the edge nodes' to attribute.
deserializeChildId2 :: Content i -> Maybe (AlgNode, AlgNode)
deserializeChildId2 node = case childIdList of 
        [edgeNode1, edgeNode2] -> liftM2 (,) edgeNode1 edgeNode2
        _                      -> Nothing
  where childIdList = map (\x -> readMaybe =<< getNodeAttribute "to" x)
                          $ (childrenBy $ tag "edge") node

-- | Try to get the content child node of another node.
deserializeContentNode :: Content i -> Maybe (Content i)
deserializeContentNode node = getSingletonChildByTag "content" node

deserializeEmptyBinaryOpGeneric :: Content i
                                -> (() -> A.BinOp)
                                -> Maybe A.PFAlgebra
deserializeEmptyBinaryOpGeneric node constructor = do
    (childId1, childId2) <- deserializeChildId2 node
    return $ BinOp (constructor ()) childId1 childId2

deserializeCross :: Content i -> Maybe A.PFAlgebra
deserializeCross node = deserializeEmptyBinaryOpGeneric node A.Cross

deserializeEqJoin :: Content i -> Maybe A.PFAlgebra
deserializeEqJoin node = do
    (childId1, childId2) <- deserializeChildId2 node
    
    contentNode <- deserializeContentNode node
    
    infEqJoin <- deserializeBinOpPosArgs contentNode

    return $ BinOp (A.EqJoin infEqJoin) childId1 childId2


deserializeThetaJoin :: Content i -> Maybe A.PFAlgebra
deserializeThetaJoin node = do
    (childId1, childId2) <- deserializeChildId2 node
    
    contentNode <- deserializeContentNode node

    -- <comparison kind="$o">
    --    <column position="1" name="$leftAttrName">
    --    <column position="2" name="$rightAttrName">
    -- </comparison>
    
    infThetaJoin <- mapM deserializeComparison
                         $ (childrenBy $ tag "comparison") contentNode

    return $ BinOp (A.ThetaJoin infThetaJoin) childId1 childId2
  where deserializeComparison :: Content i
                              -> Maybe ( A.LeftAttrName
                                       , A.RightAttrName
                                       , A.JoinRel
                                       )
        deserializeComparison compNode = do
            joinRelStr <- getNodeAttribute "kind" compNode
            joinRel <- deserializeJoinRel joinRelStr
            
            (leftAttrName, rightAttrName) <- deserializeBinOpPosArgs compNode
            
            return (leftAttrName, rightAttrName, joinRel)
            
        deserializeJoinRel :: String -> Maybe A.JoinRel
        deserializeJoinRel s = case s of
            "eq" -> return A.EqJ
            "gt" -> return A.GtJ
            "ge" -> return A.GeJ
            "lt" -> return A.LtJ
            "le" -> return A.LeJ
            "ne" -> return A.NeJ
            _    -> Nothing


deserializeDisjUnion :: Content i -> Maybe A.PFAlgebra
deserializeDisjUnion node = deserializeEmptyBinaryOpGeneric node A.DisjUnion

deserializeDifference :: Content i -> Maybe A.PFAlgebra
deserializeDifference node = deserializeEmptyBinaryOpGeneric node A.Difference


deserializeDummy :: Content i -> Maybe A.PFAlgebra
deserializeDummy node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    commentNode <- getSingletonChildByTag "comment" contentNode
    comment <- getTextChild commentNode
    
    return $ UnOp (A.Dummy comment) childId

deserializeAggr :: Content i -> Maybe A.PFAlgebra
deserializeAggr node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    -- parse aggregate nodes from contentNode
    -- read kind attribute from aggregate node
    -- depending on that, read
    --  - column function="item"
    
    aggrNodes <- mapM deserializeAggregate $ aggrFilter contentNode
    
    
    return $ UnOp ( A.Aggr ( aggrNodes
                           , deserializePartAttrName contentNode
                           )
                  )
                  childId

  where aggrFilter = childrenBy $ tag "aggregate"
        deserializeAggregate :: Content i
                             -> Maybe (A.AggrType, A.ResAttrName)
        deserializeAggregate aggregateNode = do
            aggregateStr <- getNodeAttribute "kind" aggregateNode
            
            resAttrName <- deserializeNewColumnName aggregateNode
            
            aggregate <- case aggregateStr of
                "avg"      -> return . A.Avg =<< aC
                "max"      -> return . A.Max =<< aC
                "min"      -> return . A.Min =<< aC
                "sum"      -> return . A.Sum =<< aC
                "all"      -> return . A.All =<< aC
                "prod"     -> return . A.Prod =<< aC
                "distinct" -> return . A.Dist =<< aC
                "count"    -> return A.Count
                _          -> Nothing

            return (aggregate, resAttrName)

          -- has to be lazy
          where aC = deserializeOldColumnName aggregateNode

deserializeColumnNameWithNewValue :: Content i
                                  -> String
                                  -> Maybe A.ResAttrName
deserializeColumnNameWithNewValue contentNode newValue = do
    ranColumn <- getSingletonChildByFilter ranFilter contentNode
    getNodeAttribute "name" ranColumn
  where ranFilter = tag "column"
                    `with` attrval (N "new", AttValue [Left newValue])

deserializeNewColumnName :: Content i -> Maybe A.ResAttrName
deserializeNewColumnName contentNode =
    deserializeColumnNameWithNewValue contentNode "true"

deserializeOldColumnName :: Content i -> Maybe A.AttrName
deserializeOldColumnName contentNode = do
    deserializeColumnNameWithNewValue contentNode "false"

deserializeFunBoolNot :: Content i -> Maybe A.PFAlgebra
deserializeFunBoolNot node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeNewColumnName contentNode

    attrName <- deserializeOldColumnName contentNode

    return $ UnOp (A.FunBoolNot (resAttrName, attrName))
                  childId

-- deserialize a cast operator
deserializeCast :: Content i -> Maybe A.PFAlgebra
deserializeCast node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeNewColumnName contentNode

    attrName <- deserializeOldColumnName contentNode

    typeNode <- getSingletonChildByTag "type" contentNode
    typeStr <- getNodeAttribute "name" typeNode
    type_ <- deserializeATy typeStr
    
    return $ UnOp (A.Cast (resAttrName, attrName, type_))
                  childId

deserializeBinOpResAttrName :: Content i -> Maybe A.ResAttrName
deserializeBinOpResAttrName contentNode = do
    resNode <- getSingletonChildByFilter resColumnFilter contentNode
    getNodeAttribute "name" resNode
  where resColumnFilter = tag "column" `without` attr "position"

-- deserialize positional arguments used by a BinOp
deserializeBinOpPosArgs :: Content i
                        -> Maybe (A.LeftAttrName, A.RightAttrName)
deserializeBinOpPosArgs contentNode = do
    unsortedResult <- mapM (getNodeAttributes ["position", "name"]) 
                           $ posColumnFilter contentNode

    case map (head . tail) $ sortBy (on compare head) unsortedResult of
        [lName, rName] -> return (lName, rName)
        _              -> Nothing
        
  where posColumnFilter = (childrenBy $ tag "column" `with` attr "position")

-- deserialize a binary operator with RelFun
deserializeBinOpRelFun :: Content i -> A.RelFun -> Maybe A.PFAlgebra
deserializeBinOpRelFun node relFun = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node

    resAttrName <- deserializeBinOpResAttrName contentNode

    (lName, rName) <- deserializeBinOpPosArgs contentNode

    return $ UnOp ( A.FunBinOp ( A.RelFun relFun
                               , resAttrName
                               , lName
                               , rName
                               )
                  )
                  childId

deserializeRelFun :: String -> Maybe A.RelFun
deserializeRelFun s = case s of
    "gt"  -> return A.Gt
    "lt"  -> return A.Lt
    "eq"  -> return A.Eq
    "and" -> return A.And
    "or"  -> return A.Or
    _     -> Nothing
    
-- deserialize a binary operator with Fun1to1 as function
deserializeBinOpFun :: Content i -> Maybe A.PFAlgebra
deserializeBinOpFun node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    kindNode <- getSingletonChildByFilter kindFilter contentNode
    funName <- getNodeAttribute "name" kindNode
    fun <- deserializeFun1to1 funName

    resAttrName <- deserializeBinOpResAttrName contentNode

    (lName, rName) <- deserializeBinOpPosArgs contentNode

    return $ UnOp ( A.FunBinOp ( A.Fun1to1 fun
                               , resAttrName
                               , lName
                               , rName
                               )
                  )
                  childId
  where kindFilter = (childrenBy $ tag "kind")

-- deserialize the ugly Fun1to1 results of show
deserializeFun1to1 :: String -> Maybe A.Fun1to1
deserializeFun1to1 s = case s of
    "add"           -> return A.Plus
    "subtract"      -> return A.Minus
    "multiplay"     -> return A.Times
    "divide"        -> return A.Div
    "modulo"        -> return A.Modulo
    "fn:contains"   -> return A.Contains
    "fn:similar_to" -> return A.SimilarTo
    "fn:concat"     -> return A.Concat
    _               -> Nothing

deserializeAttach :: Content i -> Maybe A.PFAlgebra
deserializeAttach node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    columnNode <- getSingletonChildByTag "column" contentNode
    
    resAttrName <- getNodeAttribute "name" columnNode
    
    valueNode <- getSingletonChildByTag "value" columnNode
    
    typeStr <- getNodeAttribute "type" valueNode
    type_ <- deserializeATy typeStr
    
    valueStr <- getTextChild valueNode
    value <- deserializeAVal type_ valueStr
    
    return $ UnOp (A.Attach (resAttrName, (type_, value))) childId

deserializeDistinct :: Content i -> Maybe A.PFAlgebra
deserializeDistinct node = do
    childId <- deserializeChildId1 node
    return $ UnOp (A.Distinct ()) childId

deserializePosSel :: Content i -> Maybe A.PFAlgebra
deserializePosSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    sortInfo <- deserializeSortInf contentNode
    
    -- TODO really plain text?
    positionNode <- getSingletonChildByTag "position" contentNode
    positionText <- getTextChild positionNode
    position <- readMaybe positionText
    
    return $ UnOp ( A.PosSel ( position
                             , sortInfo
                             , deserializePartAttrName contentNode
                             )
                  )
                  childId

deserializeSel :: Content i -> Maybe A.PFAlgebra
deserializeSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    columnNode <- getSingletonChildByTag "column" contentNode

    columnName <- getNodeAttribute "name" columnNode

    return $ UnOp (A.Sel columnName) childId 

-- | Tries to deserialize into 'Proj'.
deserializeProj :: Content i -> Maybe A.PFAlgebra
deserializeProj node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    -- FIXME what to do with columns with just the name attribute?
    projectionLists <- mapM (getNodeAttributes ["name", "old_name"])
                            $ (childrenBy $ tag "column") contentNode
    
    projectionInf <- mapM tupleConv projectionLists
    
    return $ UnOp (A.Proj $ projectionInf) childId
  where tupleConv [x, y] = return (x, y)
        tupleConv _      = Nothing

-- | Tries to deserialize a row rank or rank operator. They use the same
-- deserialize function because the only difference is the data constructor
-- itself.
deserializeRankOperator :: Content i
                        -> (A.SemInfRank -> A.UnOp)
                        -> Maybe A.PFAlgebra
deserializeRankOperator node constructor = do
    childId <- deserializeChildId1 node

    -- <content> should be singleton
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeResultAttrName contentNode
    
    sortInfo <- deserializeSortInf contentNode

    return $ UnOp (constructor ( resAttrName
                               , sortInfo
                               )
                  )
                  childId

-- | Tries to deserialize into a 'RowNum'.
deserializeRowNum :: Content i -> Maybe A.PFAlgebra
deserializeRowNum node = do

    childId <- deserializeChildId1 node

    -- <content> should be singleton
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeResultAttrName contentNode

    -- TODO string comparison sufficient ?
    sortInfo <- deserializeSortInf contentNode
    
    return $ UnOp ( A.RowNum ( resAttrName
                             , sortInfo
                             -- optional
                             , deserializePartAttrName contentNode
                             )
                  )
                  childId

-- | Tries to deserialize a 'LitTable'.
deserializeLitTable :: Content i -> Maybe A.PFAlgebra
deserializeLitTable node = do
    contentNode <- deserializeContentNode node

    let columnNodes = childrenBy (tag "column") contentNode

    results <- mapM deserializeLitTableColumn columnNodes

    -- FIXME pattern matching safe here?
    let tableContent = transpose $ map (\(_, _, vals) -> vals) results

    return $ NullaryOp $ A.LitTable tableContent
                                    $ map (\(n, t, _) -> (n, t))
                                          results

-- | Tries to deserialize a table column of a 'LitTable'.
deserializeLitTableColumn :: Content i -> Maybe (A.AttrName, A.ATy, [A.AVal])
deserializeLitTableColumn columnNode = do
    
    name <- getNodeAttribute "name" columnNode

    -- FIXME is value a single child of column in xml?
    let valueNodes = childrenBy (tag "value") columnNode
    result <- mapM deserializeLitTableValue valueNodes
    
    type_ <- the $ map fst result
    
    return (name, type_, map snd result)

-- | Tries to deserialize a value node into a tuple of 'ATy' and 'AVal'.
deserializeLitTableValue :: Content i -> Maybe (A.ATy, A.AVal)
deserializeLitTableValue valueNode = do
    typeStr <- getNodeAttribute "type" valueNode
    type_ <- deserializeATy typeStr
    
    valueStr <- getTextChild valueNode
    value <- deserializeAVal type_ valueStr

    return (type_, value)

-- | Tries to deserialize a 'TableRef'.
deserializeTableRef :: Content i -> Maybe A.PFAlgebra
deserializeTableRef node = do
    propertiesNode <- getSingletonChildByTag "properties" node
    keyInfos <- deserializeTableRefProperties propertiesNode
    
    contentElement <- deserializeContentNode node
    (tableName, attrInfo) <- deserializeTableRefContent contentElement
    
    return $ NullaryOp $ A.TableRef (tableName, attrInfo, keyInfos)

-- | Tries to deserialize the properties node into 'KeyInfos'.
deserializeTableRefProperties :: Content i -> Maybe A.KeyInfos
deserializeTableRefProperties propertiesNode = do
    -- there should only be one
    keysNode <- getSingletonChildByTag "keys" propertiesNode
    
    -- <keys><key> .. </key> .. <keys>
    keyInfos <- mapM deserializeKeyInfo
                     $ childrenBy (tag "key") keysNode
    
    return keyInfos

-- | Tries to deserializes a key node into 'KeyInfo'.
deserializeKeyInfo :: Content i -> Maybe A.KeyInfo
deserializeKeyInfo keyNode = do
    -- <key><column ..> .. </key>
    keyInfos <- mapM deserializeKeyInfoColumn
                     $ (childrenBy $ tag "column") keyNode

    -- restore ordering (based on first tuple element) and map to second
    return $ map snd $ sortBy (on compare fst) keyInfos


-- | Tries to deserialize a column node below a key node into position and name.
deserializeKeyInfoColumn :: Content i -> Maybe (Int, A.AttrName)
deserializeKeyInfoColumn columnNode = do
    -- <column name=.. position=..>
    name <- getNodeAttribute "name" columnNode
    positionStr <- getNodeAttribute "position" columnNode
    position <- readMaybe positionStr

    return (position, name)

-- | Tries to deserialize the content node in a 'TableRef'.
deserializeTableRefContent :: Content i -> Maybe (A.TableName, A.TableAttrInf)
deserializeTableRefContent contentNode = do
    tableNode <- getSingletonChildByTag "table" contentNode
    name <- getNodeAttribute "name" tableNode
    
    attributeInfo <- mapM deserializeTableRefColumn
                          $ (childrenBy $ tag "column") tableNode
    
    return (name, attributeInfo)

-- | Tries to deserialize a column node belonging to a 'TableRef'.
deserializeTableRefColumn :: Content i -> Maybe (A.AttrName, A.AttrName, A.ATy)
deserializeTableRefColumn columnNode = do

    qAttr <- getNodeAttributes ["name", "tname", "type"] columnNode

    case qAttr of
        [name, newName, typeStr] -> do
                                        type_ <- deserializeATy typeStr
                                        return (name, newName, type_)
        _                        -> Nothing

-- | Tries to deserialize an 'EmptyTable'.
deserializeEmptyTable :: Content i -> Maybe A.PFAlgebra
deserializeEmptyTable node = do

    contentNode <- deserializeContentNode node
    
    schema <- mapM deserializeEmptyTableColumn
                   $ (childrenBy $ tag "column") contentNode

    return $ NullaryOp $ A.EmptyTable schema

-- | Tries to deserialize a column node belonging to a 'EmptyTable' into
-- a tuple containing 'AttrName' and 'ATy'.
deserializeEmptyTableColumn :: Content i -> Maybe (A.AttrName, A.ATy)
deserializeEmptyTableColumn columnNode = do
    name <- getNodeAttribute "name" columnNode
    typeStr <- getNodeAttribute "type" columnNode
    type_ <- deserializeATy typeStr

    return (name, type_)


-- | Tries to deserialize a 'String' into 'Bool'.
deserializeBool :: String -> Maybe Bool
deserializeBool s = case s of
    "true"  -> return True
    "false" -> return False
    _       -> Nothing


-- | Tries to deserialize a 'String' into 'AVal'.
deserializeAVal :: A.ATy -> String -> Maybe A.AVal
deserializeAVal t s = case t of
    A.AInt    -> return . A.VInt =<< readMaybe s
    A.AStr    -> return . A.VStr =<< readMaybe s
    A.ABool   -> return . A.VBool =<< readMaybe s
    A.ADec    -> return . A.VDec =<< readMaybe s
    A.ADouble -> return . A.VDouble =<< readMaybe s
    A.ANat    -> return . A.VNat =<< readMaybe s
    -- ANat is the same as ASur, but can not be deserialized
    A.ASur    -> Nothing


-- | Tries to deserialize a 'String' into 'ATy'.
deserializeATy :: String -> Maybe A.ATy
deserializeATy s = case s of
    "int"  -> return A.AInt
    "str"  -> return A.AStr
    "bool" -> return A.ABool
    "dec"  -> return A.ADec
    "dbl"  -> return A.ADouble
    "nat"  -> return A.ANat
    -- ASur is the same as ANat
    _      -> Nothing

-- | Tries to deserialize a 'String' into 'SortDir'.
deserializeSortDir :: String -> Maybe A.SortDir
deserializeSortDir s = case s of
    "ascending"  -> return A.Asc
    "descending" -> return A.Desc
    _            -> Nothing

-- | Wraps a call to reads into a 'Maybe'.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> return x
    _         -> Nothing

-- | Checks whether every element of the list is the same and returns a 'Maybe'
-- with it. Defined in GHC.Exts but without 'Maybe'.
the :: Eq a => [a] -> Maybe a
the []     = Nothing
the (x:xs) = helper x xs
  where helper y []     = return y
        helper y (z:zs) | (y == z)  = helper z zs
                        | otherwise = Nothing

