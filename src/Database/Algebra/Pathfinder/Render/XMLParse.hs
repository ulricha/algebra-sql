
-- TODO migrate with XML ?
--module Database.Algebra.Pathfinder.Render.XMLParse
--    ( main
--    , queryNodeAttributes
--    , deserializeRowNum
--    ) where

import Control.Monad (guard, liftM2)
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.IntMap (fromList)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

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
--import Database.Algebra.Pathfinder (PFAlgebra)
import Database.Algebra.Pathfinder.Data.Algebra



-- TODO:
-- handle escaped sequences and xml entities? how does verbatim work here?
-- convert comments into haddock format


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            
            -- used for debugging purposes
            mapM_ putStrLn $ parse filename content
            
            --putStr content
            exitWith $ ExitSuccess
        _          -> do
            putStrLn "missing filename"
            exitWith $ ExitFailure 1

testParse :: IO (Content Posn)
testParse = do
    str <- readFile "../xml_plans/test.xml"
    let (Document _ _ root _) = xmlParse "" str
    return (CElem root noPos)

parse :: String -> String -> [String]
parse filename content =
    map show $ sortBy compare $ map fst $ catMaybes $ generateNodes $ nodes
  where (Document _ _ root _) = xmlParse filename content
        nodes                 = tag "query_plan_bundle" /> tag "query_plan"
                                /> tag "logical_query_plan"
                                /> tag "node" $ CElem root noPos

-- generate a list of nodes from a list of xml elements FIXME remove
generateNodes :: [Content i] -> [Maybe (AlgNode, PFAlgebra)]
-- TODO maybe use sequence here
generateNodes es = map deserializeNode es

-- FIXME ugly solution for testing purposes
deserializeQueryPlan :: String -> String -> AlgebraDag PFAlgebra
deserializeQueryPlan filename content =
    -- FIXME not safe
    mkDag (fromList tuples) [foldl min (head nodeIds) (tail nodeIds)]

  where (Document _ _ root _) = xmlParse filename content
        nodes                 = tag "query_plan_bundle" /> tag "query_plan"
                                /> tag "logical_query_plan"
                                /> tag "node" $ CElem root noPos
        tuples                = mapMaybe deserializeNode nodes
        (nodeIds, _)          = unzip tuples

-- generate a node from an xml element
deserializeNode :: Content i -> Maybe (AlgNode, PFAlgebra)
deserializeNode node@(CElem (Elem _ attributes contents) _) = do

    identifier <- lookupConvert readMaybe "id" attributes
    kind <- lookupVerbatim "kind" attributes

    result <- case kind of
        -- nullary operators
        "empty_tbl"  -> deserializeEmptyTable contents
        "table"      -> deserializeLitTable node
        "ref_tbl"    -> deserializeTableRef node
        
        -- unary operators
        "rownum"     -> deserializeRowNum node
        "rowrank"    -> deserializeRankOperator node RowRank
        "rank"       -> deserializeRankOperator node Rank
        "project"    -> deserializeProj node
        "select"     -> deserializeSel node
        "pos_sel"    -> deserializePosSel node
        "distinct"   -> deserializeDistinct node
        "attach"     -> deserializeAttach node
        "fun"        -> deserializeBinOpFun node
        
        -- unary operators with RelFun
        "gt"         -> deserializeBinOpRelFun node Gt
        "lt"         -> deserializeBinOpRelFun node Lt
        "eq"         -> deserializeBinOpRelFun node Eq
        "and"        -> deserializeBinOpRelFun node And
        "or"         -> deserializeBinOpRelFun node Or
        
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

generateNode _ = Nothing

-- queries multiple attributes from a Content adt
queryNodeAttributes :: [String] -> Content i -> Maybe [String]
queryNodeAttributes attList (CElem (Elem _ attributes _) _) =
    mapM (\att -> lookupVerbatim att attributes)
         attList

queryNodeAttributes _ _ = Nothing

-- queries all attributes of the Content adt
-- and concatenates the text node in front
queryNodeAttributesWithText :: [String] -> Content i -> Maybe [String]
queryNodeAttributesWithText attList c = do
    queriedAttributes <- queryNodeAttributes attList c
    text <- queryTextChild c
    return $ text : queriedAttributes

-- queries the text content of a Content adt
queryTextChild :: Content i -> Maybe String
queryTextChild c = do
    (CString _ charData _) <- listToMaybe $ childrenBy txt c
    return charData
    
-- queries one attribute from a Content adt
queryNodeAttribute :: String -> Content i -> Maybe String
queryNodeAttribute attName (CElem (Elem _ attributes _) _) =
    lookupVerbatim attName attributes

queryNodeAttribute _ _ = Nothing

-- assume the current node has only one child with the given tag name
-- and try to return it
querySingletonChildByTag :: String -> Content i -> Maybe (Content i)
querySingletonChildByTag tagName = listToMaybe . (childrenBy $ tag tagName)

querySingletonChildByFilter :: CFilter i -> Content i -> Maybe (Content i)
querySingletonChildByFilter f  c = listToMaybe $ childrenBy f c

-- looks up an attribute and converts the result with verbatim
lookupVerbatim :: String -> [(QName, AttValue)] -> Maybe String
lookupVerbatim = lookupConvert return

-- looks up an attribute and converts the result with the provided function
lookupConvert :: (String -> Maybe a) -> String -> [(QName, AttValue)] -> Maybe a
lookupConvert fun name attributes = fun . verbatim =<< lookup (N name) attributes

-- get the name of the result attribute from within a content node
deserializeResultAttrName :: Content i -> Maybe String
deserializeResultAttrName contentNode = do
    -- <column name=... />
    ranNode <- listToMaybe $ childrenBy ranFilter contentNode
    queryNodeAttribute "name" ranNode
  where ranFilter = tag "column" `without` attr "function"

-- gets the partition attribute name from a content node
deserializePartAttrName :: Content i -> Maybe String
deserializePartAttrName contentNode = do
    columnNode <- listToMaybe $ childrenBy panFilter contentNode
    
    queryNodeAttribute "name" columnNode
  where panFilter = tag "column"
                    `with` attrval (N "function", AttValue $ [Left "partition"])

-- get the sort information from the content node
deserializeSortInf :: Content i -> Maybe SortInf
deserializeSortInf contentNode = do
    -- <column function="sort" position=... direction=... name=... />
    let sortInfoNodes = childrenBy siFilter contentNode
    
    queriedSortInfo <- mapM (queryNodeAttributes [ "position"
                                                 , "name"
                                                 , "direction"
                                                 ]
                            )
                            sortInfoNodes

    let tupleConv :: [String] -> Maybe (SortAttrName, SortDir)
        tupleConv [_, name, dirStr] = do
            direction <- deserializeSortDir dirStr
            return (name, direction)    
        tupleConv _                 = Nothing

    mapM tupleConv $ sortBy (on compare head) queriedSortInfo
  where siFilter  = tag "column"
                    `with` attrval (N "function", AttValue $ [Left "sort"])


-- get the child id from the edge nodes to attribute
deserializeChildId1 :: Content i -> Maybe AlgNode
deserializeChildId1 node = do
    edgeNode <- querySingletonChildByTag "edge" node
    toEdge <- queryNodeAttribute "to" edgeNode
    readMaybe toEdge

-- get two child ids from the edge nodes to attribute
deserializeChildId2 :: Content i -> Maybe (AlgNode, AlgNode)
deserializeChildId2 node = case childIdList of 
        [edgeNode1, edgeNode2] -> liftM2 (,) edgeNode1 edgeNode2
        _                      -> Nothing
  where childIdList = map (\x -> readMaybe =<< queryNodeAttribute "to" x)
                          $ (childrenBy $ tag "edge") node

-- get the content node
deserializeContentNode :: Content i -> Maybe (Content i)
deserializeContentNode node = querySingletonChildByTag "content" node

deserializeEmptyBinaryOpGeneric :: Content i -> (() -> BinOp) -> Maybe PFAlgebra
deserializeEmptyBinaryOpGeneric node constructor = do
    (childId1, childId2) <- deserializeChildId2 node
    return $ BinOp (constructor ()) childId1 childId2

deserializeCross :: Content i -> Maybe PFAlgebra
deserializeCross node = deserializeEmptyBinaryOpGeneric node Cross

deserializeEqJoin :: Content i -> Maybe PFAlgebra
deserializeEqJoin node = do
    (childId1, childId2) <- deserializeChildId2 node
    
    contentNode <- deserializeContentNode node
    
    infEqJoin <- deserializeBinOpPosArgs contentNode

    return $ BinOp (EqJoin infEqJoin) childId1 childId2


deserializeThetaJoin :: Content i -> Maybe PFAlgebra
deserializeThetaJoin node = do
    (childId1, childId2) <- deserializeChildId2 node
    
    contentNode <- deserializeContentNode node

    -- <comparison kind="$o">
    --    <column position="1" name="$leftAttrName">
    --    <column position="2" name="$rightAttrName">
    -- </comparison>
    
    infThetaJoin <- mapM deserializeComparison
                         $ (childrenBy $ tag "comparison") contentNode

    return $ BinOp (ThetaJoin infThetaJoin) childId1 childId2
  where deserializeComparison :: Content i
                              -> Maybe (LeftAttrName, RightAttrName, JoinRel)
        deserializeComparison compNode = do
            joinRelStr <- queryNodeAttribute "kind" compNode
            joinRel <- deserializeJoinRel joinRelStr
            
            (leftAttrName, rightAttrName) <- deserializeBinOpPosArgs compNode
            
            return (leftAttrName, rightAttrName, joinRel)
            
        deserializeJoinRel :: String -> Maybe JoinRel
        deserializeJoinRel s = case s of
            "eq" -> return EqJ
            "gt" -> return GtJ
            "ge" -> return GeJ
            "lt" -> return LtJ
            "le" -> return LeJ
            "ne" -> return NeJ
            _    -> Nothing


deserializeDisjUnion :: Content i -> Maybe PFAlgebra
deserializeDisjUnion node = deserializeEmptyBinaryOpGeneric node DisjUnion

deserializeDifference :: Content i -> Maybe PFAlgebra
deserializeDifference node = deserializeEmptyBinaryOpGeneric node Difference


deserializeDummy :: Content i -> Maybe PFAlgebra
deserializeDummy node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    commentNode <- querySingletonChildByTag "comment" contentNode
    comment <- queryTextChild commentNode
    
    return $ UnOp (Dummy comment) childId

deserializeAggr :: Content i -> Maybe PFAlgebra
deserializeAggr node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    -- parse aggregate nodes from contentNode
    -- read kind attribute from aggregate node
    -- depending on that read
    --  - column function="item"
    
    aggrNodes <- mapM deserializeAggregate $ aggrFilter contentNode
    
    
    return $ UnOp ( Aggr ( aggrNodes
                         , deserializePartAttrName contentNode
                         )
                  )
                  childId
                  
  where aggrFilter = childrenBy $ tag "aggregate"
        deserializeAggregate :: Content i -> Maybe (AggrType, ResAttrName)
        deserializeAggregate aggregateNode = do
            aggregateStr <- queryNodeAttribute "kind" aggregateNode
            
            resAttrName <- deserializeNewColumnName aggregateNode
            
            aggregate <- case aggregateStr of
                "avg"      -> return . Avg =<< aC
                "max"      -> return . Max =<< aC
                "min"      -> return . Min =<< aC
                "sum"      -> return . Sum =<< aC
                "all"      -> return . All =<< aC
                "prod"     -> return . Prod =<< aC
                "distinct" -> return . Dist =<< aC
                "count"    -> return Count
                _          -> Nothing

            return (aggregate, resAttrName)
                
          where aC = deserializeOldColumnName aggregateNode
            

-- TODO abstract with deserializeOldColumnName
deserializeNewColumnName :: Content i -> Maybe ResAttrName
deserializeNewColumnName contentNode = do
    ranColumn <- querySingletonChildByFilter ranFilter contentNode
    queryNodeAttribute "name" ranColumn
  where ranFilter = tag "column"
                    `with` attrval (N "new", AttValue [Left "true"])

deserializeOldColumnName :: Content i -> Maybe AttrName
deserializeOldColumnName contentNode = do
    ranColumn <- querySingletonChildByFilter anFilter contentNode
    queryNodeAttribute "name" ranColumn
  where anFilter = tag "column"
                   `with` attrval (N "new", AttValue [Left "false"])

deserializeFunBoolNot :: Content i -> Maybe PFAlgebra
deserializeFunBoolNot node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeNewColumnName contentNode

    attrName <- deserializeOldColumnName contentNode

    return $ UnOp (FunBoolNot (resAttrName, attrName))
                  childId

-- deserialize a cast operator
deserializeCast :: Content i -> Maybe PFAlgebra
deserializeCast node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeNewColumnName contentNode

    attrName <- deserializeOldColumnName contentNode

    typeNode <- querySingletonChildByTag "type" contentNode
    typeStr <- queryNodeAttribute "name" typeNode
    type_ <- deserializeATy typeStr
    
    return $ UnOp (Cast (resAttrName, attrName, type_))
                  childId

deserializeBinOpResAttrName :: Content i -> Maybe ResAttrName
deserializeBinOpResAttrName contentNode = do
    resNode <- querySingletonChildByFilter resColumnFilter contentNode
    queryNodeAttribute "name" resNode
  where resColumnFilter = tag "column" `without` attr "position"

-- deserialize positional arguments used by a BinOp
deserializeBinOpPosArgs :: Content i -> Maybe (LeftAttrName, RightAttrName)
deserializeBinOpPosArgs contentNode = do
    unsortedResult <- mapM (queryNodeAttributes ["position", "name"]) 
                           $ posColumnFilter contentNode

    case map (head . tail) $ sortBy (on compare head) unsortedResult of
        [lName, rName] -> return (lName, rName)
        _              -> Nothing
        
  where posColumnFilter = (childrenBy $ tag "column" `with` attr "position")

-- deserialize a binary operator with RelFun
deserializeBinOpRelFun :: Content i -> RelFun -> Maybe PFAlgebra
deserializeBinOpRelFun node relFun = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node

    resAttrName <- deserializeBinOpResAttrName contentNode

    (lName, rName) <- deserializeBinOpPosArgs contentNode

    return $ UnOp (FunBinOp ( RelFun relFun
                            , resAttrName
                            , lName
                            , rName
                            )
                  )
                  childId


deserializeRelFun :: String -> Maybe RelFun
deserializeRelFun s = case s of
    "gt"  -> return Gt
    "lt"  -> return Lt
    "eq"  -> return Eq
    "and" -> return And
    "or"  -> return Or
    _     -> Nothing
    
-- deserialize a binary operator with Fun1to1 as function
deserializeBinOpFun :: Content i -> Maybe PFAlgebra
deserializeBinOpFun node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    kindNode <- querySingletonChildByFilter kindFilter contentNode
    funName <- queryNodeAttribute "name" kindNode
    fun <- deserializeFun1to1 funName

    resAttrName <- deserializeBinOpResAttrName contentNode

    (lName, rName) <- deserializeBinOpPosArgs contentNode

    return $ UnOp (FunBinOp ( Fun1to1 fun
                            , resAttrName
                            , lName
                            , rName
                            )
                  )
                  childId

  where kindFilter = (childrenBy $ tag "kind")

-- deserialize the ugly Fun1to1 results of show
deserializeFun1to1 :: String -> Maybe Fun1to1
deserializeFun1to1 s = case s of
    "add"           -> return Plus
    "subtract"      -> return Minus
    "multiplay"     -> return Times
    "divide"        -> return Div
    "modulo"        -> return Modulo
    "fn:contains"   -> return Contains
    "fn:similar_to" -> return SimilarTo
    "fn:concat"     -> return Concat
    _               -> Nothing

-- deserialize an attachment operator
deserializeAttach :: Content i -> Maybe PFAlgebra
deserializeAttach node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    columnNode <- querySingletonChildByTag "column" contentNode
    
    resAttrName <- queryNodeAttribute "name" columnNode
    
    valueNode <- querySingletonChildByTag "value" columnNode
    
    typeStr <- queryNodeAttribute "type" valueNode
    type_ <- deserializeATy typeStr
    
    valueStr <- queryTextChild valueNode
    value <- deserializeAVal type_ valueStr
    
    return $ UnOp (Attach (resAttrName, (type_, value))) childId

deserializeDistinct :: Content i -> Maybe PFAlgebra
deserializeDistinct node = do
    childId <- deserializeChildId1 node
    return $ UnOp (Distinct ()) childId

deserializePosSel :: Content i -> Maybe PFAlgebra
deserializePosSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    sortInfo <- deserializeSortInf contentNode
    
    -- TODO really plain text?
    positionNode <- querySingletonChildByTag "position" contentNode
    positionText <- queryTextChild positionNode
    position <- readMaybe positionText
    
    return $ UnOp (PosSel ( position
                          , sortInfo
                          , deserializePartAttrName contentNode
                          )
                  )
                  childId

deserializeSel :: Content i -> Maybe PFAlgebra
deserializeSel node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    columnNode <- querySingletonChildByTag "column" contentNode

    columnName <- queryNodeAttribute "name" columnNode

    return $ UnOp (Sel columnName) childId 

-- deserialize a projection operator
deserializeProj :: Content i -> Maybe PFAlgebra
deserializeProj node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    -- FIXME what to do with columns with just the name attribute?
    projectionLists <- mapM (queryNodeAttributes ["name", "old_name"])
                            $ (childrenBy $ tag "column") contentNode
    
    projectionInf <- mapM tupleConv projectionLists
    
    return $ UnOp (Proj $ projectionInf) childId
  where tupleConv [x, y] = return (x, y)
        tupleConv _      = Nothing

-- deserialize a row rank or rank operator
deserializeRankOperator :: Content i -> (SemInfRank -> UnOp) -> Maybe PFAlgebra
deserializeRankOperator node@(CElem _ _) constructor = do

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

deserializeRankOperator _ _ = Nothing

-- deserialize a row num operator from xml
deserializeRowNum :: Content i -> Maybe PFAlgebra
deserializeRowNum node@(CElem _ _) = do

    childId <- deserializeChildId1 node

    -- <content> should be singleton
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeResultAttrName contentNode

    -- TODO string comparison sufficient ?
    sortInfo <- deserializeSortInf contentNode
    
    return $ UnOp (RowNum ( resAttrName
                          , sortInfo
                          -- optional
                          , deserializePartAttrName contentNode
                          )
                  )
                  childId

deserializeRowNum _ = Nothing

-- deserialize the content element below the row num node
--deserializeRowNumContent :: Content


--
deserializeLitTable :: Content i -> Maybe PFAlgebra
deserializeLitTable node@(CElem _ _) = do
    contentNode <- deserializeContentNode node

    let columnNodes = childrenBy (tag "column") contentNode

    results <- mapM deserializeLitTableColumn columnNodes
    
    return $ NullaryOp $ LitTable (transpose $ map (\(_, _, vals) -> vals)
                                                   results
                                  )
                                  $ map (\(n, t, _) -> (n, t))
                                        results

deserializeLitTable _ = Nothing

-- TODO rename
-- merges all types in the column and returns a list of values
deserializeLitTableColumn :: Content i -> Maybe (AttrName, ATy, [AVal])
deserializeLitTableColumn c@(CElem (Elem _ attributes _) _) = do
    
    name <- lookupVerbatim "name" attributes

    -- FIXME is value a single child of column in xml?
    result <- mapM deserializeLitTableValue valueNodes
    
    type_ <- the $ map fst result
    
    return (name, type_, map snd result)

  where valueNodes = childrenBy (tag "value") c

deserializeColumn _ = Nothing

-- deserialize an xml value element into type and value
deserializeLitTableValue :: Content i -> Maybe (ATy, AVal)
deserializeLitTableValue (CElem (Elem _ attributes [CString _ valueString _]) _) = do
    type_ <- lookupConvert deserializeATy "type" attributes
    
    value <- deserializeAVal type_ valueString

    return (type_, value)

deserializeLitTableValue _ = Nothing

-- try to deserialize a table reference
deserializeTableRef :: Content i -> Maybe PFAlgebra
deserializeTableRef node = do
    propertyElement <- querySingletonChildByTag "properties" node
    keyInfos <- deserializeTableRefProperties propertyElement
    
    contentElement <- deserializeContentNode node
    (tableName, attrInfo) <- deserializeTableRefContent contentElement
    
    return $ NullaryOp $ TableRef (tableName, attrInfo, keyInfos)

-- extracts KeyInfos from the properties tag
deserializeTableRefProperties :: Content i -> Maybe KeyInfos
deserializeTableRefProperties e@(CElem _ _) = do
    -- there should only be one
    keysNode <- listToMaybe keysNodes
    
    -- select key nodes from keys and try to extract each key
    -- FIXME too strict?
    keyInfos <- mapM deserializeKeyInfo
                     $ childrenBy (tag "key") keysNode
    
    return keyInfos

  where keysNodes = (childrenBy $ tag "keys") e

deserializeTableRefProperties _ = Nothing

-- deserializes <key><column..> .. </key> into KeyInfos type
deserializeKeyInfo :: Content i -> Maybe KeyInfo
deserializeKeyInfo e@(CElem _ _) = do
    -- FIXME throw out incomplete keys?
    keyInfos <- mapM deserializeKeyInfoColumn columnNodes

    -- restore ordering (based ony first tuple part) and map to snd    
    return $ map snd $ sortBy (on compare fst) keyInfos

  where columnNodes = (childrenBy $ tag "column") e

deserializeKeyInfo _ = Nothing

-- deserialize an xml column element below a key element into position and name
deserializeKeyInfoColumn :: Content i -> Maybe (Int, AttrName)
deserializeKeyInfoColumn (CElem (Elem _ attributes _) _) = do
    name <- lookupVerbatim "name" attributes
    position <- lookupConvert readMaybe "position" attributes

    return (position, name)

deserializeKeyInfoColumn _ = Nothing

-- deserialize the content element in a table reference node
deserializeTableRefContent :: Content i -> Maybe (TableName, TableAttrInf)
deserializeTableRefContent c = do
    tableNode@(CElem (Elem _ attributes _) _) <- listToMaybe tableNodes

    name <- lookupVerbatim "name" attributes
    
    let columnNodes = (childrenBy $ tag "column") tableNode
    
    attributeInfo <- mapM deserializeTableRefColumn columnNodes
    
    -- FIXME sequence to strict?
    return (name, attributeInfo)
    
  where tableNodes = (childrenBy $ tag "table") c

deserializeTableRefColumn :: Content i -> Maybe (AttrName, AttrName, ATy)
deserializeTableRefColumn (CElem (Elem _ attributes _) _) = do
    name <- lookupVerbatim "name" attributes
    newName <- lookupVerbatim "tname" attributes
    type_ <- lookupConvert deserializeATy "type" attributes

    return (name, newName, type_)

deserializeTableRefColumn _ = Nothing

-- try to deserialize an empty table
-- TODO use a more general version for abstraction
deserializeEmptyTable :: [Content i] -> Maybe PFAlgebra
deserializeEmptyTable contents = do
    -- FIXME sequence maybe to strict here ? try mapMaybe otherwise
    schema <- mapM process contents
    return $ NullaryOp $ EmptyTable schema
    
  where process (CElem e _) = return . fst =<< deserializeEmptyTableColumn e
        process _ = Nothing


-- gets hopefully a HaXml Element with a "column" node
-- (used inside an empty table) and
--     returns the attribute name and type of the column
--             (also the serialized new argument) wrapped in a Just on success
--     returns Nothing otherwise
--
deserializeEmptyTableColumn :: Element i -> Maybe ((AttrName, ATy), Bool)
deserializeEmptyTableColumn (Elem qname attributes _) = do

    -- ensure that we found a column
    guard $ "column" == localName qname

    name <- lookupVerbatim "name" attributes
    new <- lookupConvert deserializeBool "new" attributes
    type_ <- lookupConvert deserializeATy "type" attributes

    return ((name, type_), new)
    

-- deserialize a boolean string (from XML version)
deserializeBool :: String -> Maybe Bool
deserializeBool s = case s of
    "true"  -> return True
    "false" -> return False
    _       -> Nothing


-- deserialize an AVal with a given ATy
deserializeAVal :: ATy -> String -> Maybe AVal
deserializeAVal t s = case t of
    AInt    -> return . VInt =<< readMaybe s
    AStr    -> return . VStr =<< readMaybe s
    ABool   -> return . VBool =<< readMaybe s
    ADec    -> return . VDec =<< readMaybe s
    ADouble -> return . VDouble =<< readMaybe s
    ANat    -> return . VNat =<< readMaybe s
    -- FIXME not used because no value type available?
    ASur    -> Nothing

-- deserialize a ATy from a string (from XML version)
-- FIXME there should really be an instance of Read ATy ...
deserializeATy :: String -> Maybe ATy
deserializeATy s = case s of
    "int"  -> return AInt
    "str"  -> return AStr
    "bool" -> return ABool
    "dec"  -> return ADec
    "dbl"  -> return ADouble
    "nat"  -> return ANat
    -- FIXME "nat" is also used for ASur, another mistake ?
    _      -> Nothing

deserializeSortDir :: String -> Maybe SortDir
deserializeSortDir s = case s of
    "ascending"  -> return Asc
    "descending" -> return Desc
    _            -> Nothing

-- FIXME may be defined in ghc 7.6 ?
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> return x
    _         -> Nothing
    
-- FIXME defined in GHC.Exts but without Maybe
the :: Eq a => [a] -> Maybe a
the []     = Nothing
the (x:xs) = helper x xs
  where helper y []     = return y
        helper y (z:zs) | (y == z)  = helper z zs
                        | otherwise = Nothing

