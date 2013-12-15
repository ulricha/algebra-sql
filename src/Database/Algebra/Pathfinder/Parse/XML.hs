module Database.Algebra.Pathfinder.Parse.XML
    ( deserializeQueryPlan
    , deserializeQueryPlanIncomplete
    ) where

import Control.Monad (liftM2, (>=>))
import Control.Monad.Error.Class (throwError, catchError)
import Control.Error (hush)
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.IntMap (fromList)
import Data.List (sortBy, transpose)
import Data.Maybe ()

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Posn (noPos, Posn)
import Text.XML.HaXml.Types ( Element (Elem)
                            , Document (Document)
                            , Content (CElem, CString)
                            , QName (N)
                            , AttValue (AttValue)
                            , info
                            )
import Text.XML.HaXml.Combinators ( CFilter
                                  , (/>)
                                  , attr
                                  , attrval
                                  , childrenBy
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
-- fix top level node deserializer function comments
-- make comments more precise about what is meant by 'node'
-- use xmlUnEscape? (see XML.hs for locations)

type Failable = Either String

-- | The path where the actual nodes are.
xmlStartingPathFilter :: (Show i) => CFilter i
xmlStartingPathFilter = tag "query_plan_bundle"
                        /> tag "query_plan"
                        /> tag "logical_query_plan"
                        /> tag "node"

-- | Tries to deserialize a query plan. Can fail due to structural errors.
deserializeQueryPlan :: String -- ^ Filename used for error reporting.
                     -> String -- ^ Content of the file.
                     -> Either [String] (AlgebraDag A.PFAlgebra)
deserializeQueryPlan filename content =
    xmlNodesToQueryPlan $ parseXml filename content

-- | Tries to deserialize a query plan, but build a graph with succesful
-- results.
deserializeQueryPlanIncomplete :: String
                               -> String
                               -> (AlgebraDag A.PFAlgebra, [String])
deserializeQueryPlanIncomplete filename content =
    xmlNodesToQueryPlanIncomplete $ parseXml filename content

-- | Parse the given string.
parseXml :: String     -- ^ Filename used for error reporting
         -> String     -- ^ Content to parse.
         -> [Content Posn]  -- ^ The nodes we actually need.
parseXml filename content = xmlStartingPathFilter $ CElem root noPos
  where (Document _ _ root _) = xmlParse filename content

-- | Reads all nodes and only creates a DAG when there were no errors.
xmlNodesToQueryPlan :: Show i
                    => [Content i]
                    -> Either [String] (AlgebraDag A.PFAlgebra)
xmlNodesToQueryPlan xmlNodes =
    case optDag of
        Right d -> return d
        Left _  -> Left (lefts results)
  where results :: [Either String (AlgNode, A.PFAlgebra)]
        results = map deserializeNode xmlNodes
        optDag  = do
            tuples <- sequence results
            return $ constructDag tuples

-- | Reads all nodes which can be read and packs them into a
-- graph (but the graph can be wrong).
xmlNodesToQueryPlanIncomplete :: Show i
                              => [Content i]
                                 -- The DAG and a list of errors.
                              -> (AlgebraDag A.PFAlgebra, [String])
xmlNodesToQueryPlanIncomplete xmlNodes = (constructDag tuples, errors)
  where result = map deserializeNode xmlNodes
        tuples = rights result
        errors = lefts result

-- | Select the root nodes and create the DAG.
constructDag :: [(AlgNode, A.PFAlgebra)]
             -> AlgebraDag A.PFAlgebra
constructDag mapping =
    mkDag (fromList mapping')
          $ case mapping' of
                x:_ -> [fst x]
                _   -> []
  where mapping' = drop 2 $ reverse mapping

-- | Generate a node from an xml element.
deserializeNode :: Show i => Content i -> Failable (AlgNode, A.PFAlgebra)
deserializeNode node@(CElem (Elem _ attributes _) _) = do

    identifier <- lookupConvert readMonadic "id" attributes
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

        -- top level wrapper nodes (TODO what are these for?)
        -- since they are not referenced they get thrown away
        "nil"        -> return $ NullaryOp $ A.EmptyTable []
        "serialize relation"
                     -> do

            -- ignore nil id
            (_, rootId) <- deserializeChildId2 node

            -- Since we can't return something useful here just return a dummy
            -- which wraps arround the root node.
            return $ UnOp (A.Dummy "root node wrapper") rootId


        _  -> throwError $ "invalid kind attribute of node with id '"
                           ++ show identifier ++ "' at " ++ strInfo node

    return (identifier, result)

deserializeNode n                                    =
    fail $ "expected node to be a xml tag at " ++ strInfo n

-- | Queries multiple attributes from a given node.
getNodeAttributes :: Show i => [String] -> Content i -> Failable [String]
getNodeAttributes attList (CElem (Elem _ attributes _) _) =
    mapM (\att -> lookupVerbatim att attributes)
         attList

getNodeAttributes _ n =
    throwError $ "xml content has no attributes at " ++ strInfo n

-- | Queries the text content of a given node.
getTextChild :: Show i => Content i -> Failable String
getTextChild c =
    case childrenBy txt c of
        [CString _ charData _] -> return charData
        _                      ->
            throwError $ "no text child found at " ++ strInfo c
    
-- | Queries one attribute from a given node.
getNodeAttribute :: Show i => String -> Content i -> Failable String
getNodeAttribute attName (CElem (Elem _ attributes _) _) =
    lookupVerbatim attName attributes

getNodeAttribute _ n                                       =
    throwError $ "xml content has no attributes at " ++ strInfo n

-- | Append information to an already existing error message. Used as handler
-- for 'catchError'.
handleConsecutiveError :: Show i
                       => String
                       -> Content i
                       -> String
                       -> Failable a
handleConsecutiveError msg node e =
    throwError $ msg ++ " at " ++ strInfo node ++ ": " ++ e

-- | Assume the current node has only one child with the given tag name
-- and try to return it.
getSingletonChildByTag :: Show i => String -> Content i -> Failable (Content i)
getSingletonChildByTag tagName node =
    asSingleton (childrenBy (tag tagName) node)
    `catchError` handleConsecutiveError
                 ( "getSingletonChildByTag with tag '"
                   ++ tagName
                 )
                 node

-- | Same as 'getSingletonChildByTag' but with a filter.
getSingletonChildByFilter :: Show i
                          => CFilter i
                          -> Content i
                          -> Failable (Content i)
getSingletonChildByFilter f c =
    asSingleton (childrenBy f c)
    `catchError` (throwError . ("getSingletonChildByFilter: " ++))

asSingleton :: [a] -> Failable a
asSingleton []  = throwError "empty list found"
asSingleton [x] = return x
asSingleton _   = throwError "list with length greater 1 found"

-- | Looks up an attribute and returns it as a 'String'.
lookupVerbatim :: String -> [(QName, AttValue)] -> Failable String
lookupVerbatim = lookupConvert return

-- | Looks up an attribute and converts the result with the provided function.
lookupConvert :: (String -> Failable a) -- ^ The function to convert with
              -> String                 -- ^ Name of the attribute
              -> [(QName, AttValue)]    -- ^ The attribute mapping
              -> Failable a
lookupConvert fun name attributes = do
    attValStr <- case lookup (N name) attributes of
        Just x  -> return x
        Nothing -> throwError $ "did not find attribute '" ++ name
                                ++ "' in association list"
    fun $ verbatim attValStr

-- | Tries to get the name of the result attribute from within a content node.
deserializeResultAttrName :: Show i => Content i -> Failable String
deserializeResultAttrName contentNode = do
    -- <column name=... />
    ranNode <- getSingletonChildByFilter ranFilter contentNode
               `catchError` handleConsecutiveError
                            "expected exactly one column tag without the\
                            \ function attribute"
                            contentNode

    getNodeAttribute "name" ranNode
  where ranFilter = tag "column" `without` attr "function"

-- | Tries to get the partition attribute name from within a content node.
deserializePartAttrName :: Show i => Content i -> Failable String
deserializePartAttrName contentNode = do
    columnNode <- getSingletonChildByFilter panFilter contentNode
                  `catchError` handleConsecutiveError
                               "expected exactly one column tag without the\
                               \ function=partition attribute"
                               contentNode
    getNodeAttribute "name" columnNode
  where panFilter = tag "column"
                    `with` attrval (N "function", AttValue [Left "partition"])

-- | Tries to get the sort information from the content node.
deserializeSortInf :: Show i => Content i -> Failable A.SortInf
deserializeSortInf contentNode = do
    -- <column function="sort" position=... direction=... name=... />
    let sortInfoNodes = childrenBy siFilter contentNode
    
    queriedSortInfo <- mapM (getNodeAttributes [ "position"
                                               , "name"
                                               , "direction"
                                               ]
                            )
                            sortInfoNodes

    let tupleConv :: [String] -> Failable (A.SortAttrName, A.SortDir)
        tupleConv [_, name, dirStr] = do
            direction <- deserializeSortDir dirStr
            return (name, direction)    
        tupleConv _                 =
            throwError $ "invalid sort information at " ++ strInfo contentNode

    mapM tupleConv $ sortBy (on compare head) queriedSortInfo
  where siFilter  = tag "column"
                    `with` attrval (N "function", AttValue [Left "sort"])


-- | Try to get a single child id from the edge node's to attribute.
deserializeChildId1 :: Show i => Content i -> Failable AlgNode
deserializeChildId1 node = do
    edgeNode <- getSingletonChildByTag "edge" node
    toEdge <- getNodeAttribute "to" edgeNode
    readMonadic toEdge

-- | Try to get two child ids from the edge nodes' to attribute.
deserializeChildId2 :: Show i => Content i -> Failable (AlgNode, AlgNode)
deserializeChildId2 node = case childIdList of 
        [edgeNode1, edgeNode2] -> liftM2 (,) edgeNode1 edgeNode2
        _                      ->
            throwError $ "did only expect two children at " ++ strInfo node
  where childIdList = map (getNodeAttribute "to" >=> readMonadic)
                          $ (childrenBy $ tag "edge") node

-- | Try to get the content child node of another node.
deserializeContentNode :: Show i => Content i -> Failable (Content i)
deserializeContentNode = getSingletonChildByTag "content"

deserializeEmptyBinaryOpGeneric :: Show i
                                => Content i
                                -> (() -> A.BinOp)
                                -> Failable A.PFAlgebra
deserializeEmptyBinaryOpGeneric node constructor = do
    (childId1, childId2) <- deserializeChildId2 node
    return $ BinOp (constructor ()) childId1 childId2

deserializeCross :: Show i => Content i -> Failable A.PFAlgebra
deserializeCross node = deserializeEmptyBinaryOpGeneric node A.Cross

deserializeEqJoin :: Show i => Content i -> Failable A.PFAlgebra
deserializeEqJoin node = do
    (childId1, childId2) <- deserializeChildId2 node
    
    contentNode <- deserializeContentNode node
    
    infEqJoin <- deserializeBinOpPosArgs contentNode

    return $ BinOp (A.EqJoin infEqJoin) childId1 childId2


deserializeThetaJoin :: Show i => Content i -> Failable A.PFAlgebra
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
  where deserializeComparison :: Show i
                              => Content i
                              -> Failable ( A.LeftAttrName
                                          , A.RightAttrName
                                          , A.JoinRel
                                          )
        deserializeComparison compNode = do
            joinRelStr <- getNodeAttribute "kind" compNode
            joinRel <- deserializeJoinRel joinRelStr
            
            (leftAttrName, rightAttrName) <- deserializeBinOpPosArgs compNode
            
            return (leftAttrName, rightAttrName, joinRel)
            
        deserializeJoinRel :: String -> Failable A.JoinRel
        deserializeJoinRel s = case s of
            "eq" -> return A.EqJ
            "gt" -> return A.GtJ
            "ge" -> return A.GeJ
            "lt" -> return A.LtJ
            "le" -> return A.LeJ
            "ne" -> return A.NeJ
            n    -> throwError $ "invalid JoinRel value '"
                                 ++ n
                                 ++ "' at "
                                 ++ strInfo node

deserializeDisjUnion :: Show i => Content i -> Failable A.PFAlgebra
deserializeDisjUnion node = deserializeEmptyBinaryOpGeneric node A.DisjUnion

deserializeDifference :: Show i => Content i -> Failable A.PFAlgebra
deserializeDifference node = deserializeEmptyBinaryOpGeneric node A.Difference


deserializeDummy :: Show i => Content i -> Failable A.PFAlgebra
deserializeDummy node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    commentNode <- getSingletonChildByTag "comment" contentNode
    comment <- getTextChild commentNode
    
    return $ UnOp (A.Dummy comment) childId

deserializeAggr :: Show i => Content i -> Failable A.PFAlgebra
deserializeAggr node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    -- parse aggregate nodes from contentNode
    -- read kind attribute from aggregate node
    -- depending on that, read
    --  - column function="item"
    
    aggrNodes <- mapM deserializeAggregate $ aggrFilter contentNode
    
    
    return $ UnOp ( A.Aggr ( aggrNodes
                           , hush $ deserializePartAttrName contentNode
                           )
                  )
                  childId

  where aggrFilter = childrenBy $ tag "aggregate"
        deserializeAggregate :: Show i
                             => Content i
                             -> Failable (A.AggrType, A.ResAttrName)
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
                n          ->
                    throwError $ "invalid aggregate function name '"
                                 ++ n
                                 ++ "' at "
                                 ++ strInfo node

            return (aggregate, resAttrName)

          -- has to be lazy
          where aC = deserializeOldColumnName aggregateNode

deserializeColumnNameWithNewValue :: Show i
                                  => Content i
                                  -> String
                                  -> Failable A.ResAttrName
deserializeColumnNameWithNewValue contentNode newValue = do
    ranColumn <- getSingletonChildByFilter ranFilter contentNode
                 `catchError` handler
    getNodeAttribute "name" ranColumn
  where ranFilter = tag "column"
                    `with` attrval (N "new", AttValue [Left newValue])
        handler = handleConsecutiveError
                  ( "expected exactly one column tag with\
                    \ the new attribute set to '"
                    ++ newValue
                  )
                  contentNode

deserializeNewColumnName :: Show i => Content i -> Failable A.ResAttrName
deserializeNewColumnName contentNode =
    deserializeColumnNameWithNewValue contentNode "true"

deserializeOldColumnName :: Show i => Content i -> Failable A.AttrName
deserializeOldColumnName contentNode =
    deserializeColumnNameWithNewValue contentNode "false"

deserializeFunBoolNot :: Show i => Content i -> Failable A.PFAlgebra
deserializeFunBoolNot node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeNewColumnName contentNode

    attrName <- deserializeOldColumnName contentNode

    return $ UnOp (A.FunBoolNot (resAttrName, attrName))
                  childId

-- deserialize a cast operator
deserializeCast :: Show i => Content i -> Failable A.PFAlgebra
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

deserializeBinOpResAttrName :: Show i => Content i -> Failable A.ResAttrName
deserializeBinOpResAttrName contentNode = do
    resNode <- getSingletonChildByFilter resColumnFilter contentNode
               `catchError` handleConsecutiveError
                            "expected exactly one column tag with\
                            \ attribute position" 
                            contentNode
    getNodeAttribute "name" resNode
  where resColumnFilter = tag "column" `without` attr "position"

-- deserialize positional arguments used by a BinOp
deserializeBinOpPosArgs :: Show i
                        => Content i
                        -> Failable (A.LeftAttrName, A.RightAttrName)
deserializeBinOpPosArgs contentNode = do
    unsortedResult <- mapM (getNodeAttributes ["position", "name"]) 
                           $ posColumnFilter contentNode

    case map (head . tail) $ sortBy (on compare head) unsortedResult of
        [lName, rName] -> return (lName, rName)
                          -- TODO better error message
        _              -> throwError "invalid number of attributes deserialized"
        
  where posColumnFilter = childrenBy (tag "column" `with` attr "position")

-- deserialize a binary operator with RelFun
deserializeBinOpRelFun :: Show i
                       => Content i
                       -> A.RelFun
                       -> Failable A.PFAlgebra
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

-- deserialize a binary operator with Fun1to1 as function
deserializeBinOpFun :: Show i => Content i -> Failable A.PFAlgebra
deserializeBinOpFun node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node

    kindNode <- getSingletonChildByTag "kind" contentNode
                `catchError` handleConsecutiveError
                             "kind tag not found at"
                             contentNode

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

  -- deserialize the ugly Fun1to1 results of show
deserializeFun1to1 :: String -> Failable A.Fun1to1
deserializeFun1to1 s = case s of
    "add"           -> return A.Plus
    "subtract"      -> return A.Minus
    "multiplay"     -> return A.Times
    "divide"        -> return A.Div
    "modulo"        -> return A.Modulo
    "fn:contains"   -> return A.Contains
    "fn:similar_to" -> return A.SimilarTo
    "fn:concat"     -> return A.Concat
    n               -> throwError $ "invalid Fun1to1 name '" ++ n ++ "' found"

deserializeAttach :: Show i => Content i -> Failable A.PFAlgebra
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

deserializeDistinct :: Show i => Content i -> Failable A.PFAlgebra
deserializeDistinct node = do
    childId <- deserializeChildId1 node
    return $ UnOp (A.Distinct ()) childId

deserializePosSel :: Show i => Content i -> Failable A.PFAlgebra
deserializePosSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    sortInfo <- deserializeSortInf contentNode
    
    -- TODO really plain text?
    positionNode <- getSingletonChildByTag "position" contentNode
    positionText <- getTextChild positionNode
    position <- readMonadic positionText
    
    return $ UnOp ( A.PosSel ( position
                             , sortInfo
                             , hush $ deserializePartAttrName contentNode
                             )
                  )
                  childId

deserializeSel :: Show i => Content i -> Failable A.PFAlgebra
deserializeSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    columnNode <- getSingletonChildByTag "column" contentNode

    columnName <- getNodeAttribute "name" columnNode

    return $ UnOp (A.Sel columnName) childId 

-- | Tries to deserialize into 'Proj'.
deserializeProj :: Show i => Content i -> Failable A.PFAlgebra
deserializeProj node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    projectionInf <- mapM f $ (childrenBy $ tag "column") contentNode

    return $ UnOp (A.Proj projectionInf) childId
  where f c = do
            new <- getNodeAttribute "new" c 
            case new of
                -- indicates that there is old_name
                "true"  -> do
                    [n, o] <- getNodeAttributes ["name", "old_name"] c
                    return (n, o)
                -- indicates that source equals target name
                "false" -> do
                    n <- getNodeAttribute "name" c
                    return (n, n)
                n       ->
                    throwError $ "expected true or false, got '"
                                 ++ n
                                 ++ "' at "
                                 ++ strInfo c

-- | Tries to deserialize a row rank or rank operator. They use the same
-- deserialize function because the only difference is the data constructor
-- itself.
deserializeRankOperator :: Show i
                        => Content i
                        -> (A.SemInfRank -> A.UnOp)
                        -> Failable A.PFAlgebra
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
deserializeRowNum :: Show i => Content i -> Failable A.PFAlgebra
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
                             , hush $ deserializePartAttrName contentNode
                             )
                  )
                  childId

-- | Tries to deserialize a 'LitTable'.
deserializeLitTable :: Show i => Content i -> Failable A.PFAlgebra
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
deserializeLitTableColumn :: Show i
                          => Content i
                          -> Failable (A.AttrName, A.ATy, [A.AVal])
deserializeLitTableColumn columnNode = do
    
    name <- getNodeAttribute "name" columnNode

    -- FIXME is value a single child of column in xml?
    let valueNodes = childrenBy (tag "value") columnNode
    result <- mapM deserializeLitTableValue valueNodes
    
    type_ <- the $ map fst result
    
    return (name, type_, map snd result)

-- | Tries to deserialize a value node into a tuple of 'ATy' and 'AVal'.
deserializeLitTableValue :: Show i => Content i -> Failable (A.ATy, A.AVal)
deserializeLitTableValue valueNode = do
    typeStr <- getNodeAttribute "type" valueNode
    type_ <- deserializeATy typeStr
    
    valueStr <- getTextChild valueNode
    value <- deserializeAVal type_ valueStr

    return (type_, value)

-- | Tries to deserialize a 'TableRef'.
deserializeTableRef :: Show i => Content i -> Failable A.PFAlgebra
deserializeTableRef node = do
    propertiesNode <- getSingletonChildByTag "properties" node
    keyInfos <- deserializeTableRefProperties propertiesNode
    
    contentElement <- deserializeContentNode node
    (tableName, attrInfo) <- deserializeTableRefContent contentElement
    
    return $ NullaryOp $ A.TableRef (tableName, attrInfo, keyInfos)

-- | Tries to deserialize the properties node into 'KeyInfos'.
deserializeTableRefProperties :: Show i => Content i -> Failable [A.Key]
deserializeTableRefProperties propertiesNode = do
    -- there should only be one
    keysNode <- getSingletonChildByTag "keys" propertiesNode
    
    -- <keys><key> .. </key> .. <keys>
    mapM deserializeKeyInfo $ childrenBy (tag "key") keysNode 

-- | Tries to deserializes a key node into 'KeyInfo'.
deserializeKeyInfo :: Show i => Content i -> Failable A.Key
deserializeKeyInfo keyNode = do
    -- <key><column ..> .. </key>
    keyInfos <- mapM deserializeKeyInfoColumn
                     $ (childrenBy $ tag "column") keyNode

    -- restore ordering (based on first tuple element) and map to second
    return $ A.Key $ map snd $ sortBy (on compare fst) keyInfos


-- | Tries to deserialize a column node below a key node into position and name.
deserializeKeyInfoColumn :: Show i => Content i -> Failable (Int, A.AttrName)
deserializeKeyInfoColumn columnNode = do
    -- <column name=.. position=..>
    name <- getNodeAttribute "name" columnNode
    positionStr <- getNodeAttribute "position" columnNode
    position <- readMonadic positionStr

    return (position, name)

-- | Tries to deserialize the content node in a 'TableRef'.
deserializeTableRefContent :: Show i
                           => Content i
                           -> Failable (A.TableName, A.TableAttrInf)
deserializeTableRefContent contentNode = do
    tableNode <- getSingletonChildByTag "table" contentNode
    name <- getNodeAttribute "name" tableNode
    
    attributeInfo <- mapM deserializeTableRefColumn
                          $ (childrenBy $ tag "column") tableNode
    
    return (name, attributeInfo)

-- | Tries to deserialize a column node belonging to a 'TableRef'.
deserializeTableRefColumn :: Show i
                          => Content i
                          -> Failable (A.AttrName, A.AttrName, A.ATy)
deserializeTableRefColumn columnNode = do

    qAttr <- getNodeAttributes ["tname", "name", "type"] columnNode

    case qAttr of
        [name, newName, typeStr] -> do
                                        type_ <- deserializeATy typeStr
                                        return (name, newName, type_)
        _                        ->
            throwError $ "invalid TableRef at " ++ strInfo columnNode

-- | Tries to deserialize an 'EmptyTable'.
deserializeEmptyTable :: Show i => Content i -> Failable A.PFAlgebra
deserializeEmptyTable node = do

    contentNode <- deserializeContentNode node
    
    schema <- mapM deserializeEmptyTableColumn
                   $ (childrenBy $ tag "column") contentNode

    return $ NullaryOp $ A.EmptyTable schema

-- | Tries to deserialize a column node belonging to a 'EmptyTable' into
-- a tuple containing 'AttrName' and 'ATy'.
deserializeEmptyTableColumn :: Show i
                            => Content i
                            -> Failable (A.AttrName, A.ATy)
deserializeEmptyTableColumn columnNode = do
    name <- getNodeAttribute "name" columnNode
    typeStr <- getNodeAttribute "type" columnNode
    type_ <- deserializeATy typeStr

    return (name, type_)


-- | Tries to deserialize a 'String' into 'Bool'.
deserializeBool :: String -> Failable Bool
deserializeBool s = case s of
    "true"  -> return True
    "false" -> return False
    n       -> throwError $ "invalid boolean value '" ++ n ++ "'"


-- | Tries to deserialize a 'String' into 'AVal'.
deserializeAVal :: A.ATy -> String -> Failable A.AVal
deserializeAVal t s = case t of
    A.AInt    -> return . A.VInt =<< readMonadic s
    A.AStr    -> return $ A.VStr s
    A.ABool   -> return . A.VBool =<< deserializeBool s
    A.ADec    -> return . A.VDec =<< readMonadic s
    A.ADouble -> return . A.VDouble =<< readMonadic s
    A.ANat    -> return . A.VNat =<< readMonadic s
    -- ANat is the same as ASur, but can not be deserialized
    A.ASur    -> throwError "invalid surrogate value found"


-- | Tries to deserialize a 'String' into 'ATy'.
deserializeATy :: String -> Failable A.ATy
deserializeATy s = case s of
    "int"  -> return A.AInt
    "str"  -> return A.AStr
    "bool" -> return A.ABool
    "dec"  -> return A.ADec
    "dbl"  -> return A.ADouble
    "nat"  -> return A.ANat
    -- ASur is the same as ANat
    n      -> throwError $ "invalid ATy name '" ++ n ++ "' found"

-- | Tries to deserialize a 'String' into 'SortDir'.
deserializeSortDir :: String -> Failable A.SortDir
deserializeSortDir s = case s of
    "ascending"  -> return A.Asc
    "descending" -> return A.Desc
    n            -> throwError $ "invalid SortDor value '" ++ n ++ "' found"


-- | Provide a readable string of the position.
strInfo :: Show i => Content i -> String
strInfo = show . info

-- | Wraps a call to reads into a monad (the code itself is monadic, the type is
-- limited to 'Failable').
readMonadic :: Read a => String -> Failable a
readMonadic s = case reads s of
    [(x, "")] -> return x
    _         -> throwError $ "invalid format of '" ++ s ++ "'"

-- | Checks whether every element of the list is the same and returns a monad
-- with it. Defined in GHC.Exts but without monadic context.
the :: Eq a => [a] -> Failable a
the []     = throwError "empty list"
the (x:xs) = helper x xs
  where helper y []     = return y
        helper y (z:zs) | y == z    = helper z zs
                        | otherwise = throwError "not all values are equal"

