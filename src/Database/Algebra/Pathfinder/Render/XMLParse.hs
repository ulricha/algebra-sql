
-- TODO migrate with XML ?
--module Database.Algebra.Pathfinder.Render.XMLParse
--    ( main
--    , queryNodeAttributes
--    , deserializeRowNum
--    ) where

import Control.Monad (guard, liftM2)
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Maybe (listToMaybe, mapMaybe)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Types --(Element, Document)
import Text.XML.HaXml.Namespaces (localName) -- we don't use xml namespaces
import Text.XML.HaXml.Combinators ( (/>)
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

import Database.Algebra.Dag ()
import Database.Algebra.Dag.Common (AlgNode, Algebra(NullaryOp, UnOp))
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


parse :: String -> String -> [String]
parse filename content =
    map show $ generateNodes $ nodes
  where (Document _ _ root _) = xmlParse filename content
        nodes                 = tag "query_plan_bundle" /> tag "query_plan"
                                /> tag "logical_query_plan"
                                /> tag "node" $ CElem root noPos

-- generate a list of nodes from a list of xml elements
generateNodes :: [Content i] -> [(AlgNode, PFAlgebra)]
-- TODO maybe use sequence here
generateNodes es = mapMaybe generateNode es

-- generate a node from an xml element
generateNode :: Content i -> Maybe (AlgNode, PFAlgebra)
generateNode node@(CElem (Elem _ attributes contents) _) = do
    -- FIXME read could fail, use reads
    identifier <- (lookupConvert readMaybe "id" attributes) :: Maybe Int
    kind <- lookupVerbatim "kind" attributes

    result <- case kind of
        -- nullary operators
        "empty_tbl" -> deserializeEmptyTable contents
        "table"     -> deserializeLitTable node
        "ref_tbl"   -> deserializeTableRef node
        
        -- unary operators
        "rownum"    -> deserializeRowNum node
        "rowrank"   -> deserializeRankOperator node RowRank
        "rank"      -> deserializeRankOperator node Rank
        "project"   -> deserializeProj node
        --"attach"    ->      -- mkAttachNode -> UnOp (Attach ...)
        --"eqjoin"    ->      -- mkEqJoinNode -> BinOp (EqJoin ...)
        _  -> Nothing

    return (identifier, result)

generateNode _ = Nothing

-- queries multiple attributes from a Content adt
queryNodeAttributes :: [String] -> Content i -> Maybe [String]
queryNodeAttributes attList e@(CElem (Elem _ attributes _) _) =
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

-- looks up an attribute and converts the result with verbatim
lookupVerbatim :: String -> [(QName, AttValue)] -> Maybe String
lookupVerbatim = lookupConvert Just

-- looks up an attribute and converts the result with the provided function
lookupConvert :: (String -> Maybe a) -> String -> [(QName, AttValue)] -> Maybe a
lookupConvert fun name attributes = fun . verbatim =<< lookup (N name) attributes

-- get the name of the result attribute from within a content node
deserializeResultAttrNodeName :: Content i -> Maybe String
deserializeResultAttrNodeName contentNode = do
    -- <column name=... />
    ranNode <- listToMaybe $ childrenBy ranFilter contentNode
    queryNodeAttribute "name" ranNode
  where ranFilter = tag "column" `without` attr "function"

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
    edgeNode <- listToMaybe $ (childrenBy $ tag "edge") node
    readMaybe =<< queryNodeAttribute "to" edgeNode

-- get two child ids from the edge nodes to attribute
deserializeChildId2 :: Content i -> Maybe (AlgNode, AlgNode)
deserializeChildId2 node = case childIdList of 
        [edgeNode1, edgeNode2] -> liftM2 (,) edgeNode1 edgeNode2
        _                      -> Nothing
  where childIdList = map (\x -> readMaybe =<< queryNodeAttribute "to" x)
                          $ (childrenBy $ tag "edge") node

-- get the content node
deserializeContentNode :: Content i -> Maybe (Content i)
deserializeContentNode node = listToMaybe $ (childrenBy $ tag "content") node

--type SemInfProj = ProjInf
-- type ProjInf = [ProjPair]
-- type ProjPair = (NewAttrName, OldAttrName)

-- deserialize a projection operator
deserializeProj :: Content i -> Maybe PFAlgebra
deserializeProj node = do
    childId <- deserializeChildId1 node
    
    contentNode <- deserializeContentNode node
    
    projectionLists <- mapM (queryNodeAttributes ["name", "old_name"])
                            $ (childrenBy $ tag "column") contentNode
    
    projectionInf <- mapM tupleConv projectionLists
    
    return $ UnOp (Proj $ projectionInf) childId
  where tupleConv [x, y] = Just (x, y)
        tupleConv _      = Nothing

-- deserialize a row rank or rank operator
deserializeRankOperator :: Content i -> (SemInfRank -> UnOp) -> Maybe PFAlgebra
deserializeRankOperator node@(CElem _ _) constructor = do

    childId <- deserializeChildId1 node
    
    -- <content> should be singleton
    contentNode <- deserializeContentNode node
    
    resAttrName <- deserializeResultAttrNodeName contentNode
    
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
    
    resAttrName <- deserializeResultAttrNodeName contentNode


    -- TODO string comparison sufficient ?
    sortInfo <- deserializeSortInf contentNode
    
    return $ UnOp (RowNum ( resAttrName
                          , sortInfo
                          -- optional
                          , queryNodeAttribute "name"
                            =<< (listToMaybe $ childrenBy panFilter contentNode)
                          )
                  )
                  childId

  where panFilter = tag "column"
                    `with` attrval (N "function", AttValue $ [Left "partition"])

deserializeRowNum _ = Nothing

-- deserialize the content element below the row num node
--deserializeRowNumContent :: Content


--
deserializeLitTable :: Content i -> Maybe PFAlgebra
deserializeLitTable node@(CElem _ _) = do
    contentNode <- deserializeContentNode node

    let columnNodes = childrenBy (tag "column") contentNode

    results <- sequence $ map deserializeLitTableColumn columnNodes
    
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
    result <- sequence $ map deserializeValue valueNodes
    
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

deserializeValue _ = Nothing

-- try to deserialize a table reference
deserializeTableRef :: Content i -> Maybe PFAlgebra
deserializeTableRef node = do
    propertyElement <- listToMaybe $ (childrenBy $ tag "properties") node
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
    keyInfos <- sequence $ map deserializeKeyInfo
                               $ childrenBy (tag "key") keysNode
    
    return keyInfos

  where keysNodes = (childrenBy $ tag "keys") e

deserializeTableRefProperties _ = Nothing

-- deserializes <key><column..> .. </key> into KeyInfos type
deserializeKeyInfo :: Content i -> Maybe KeyInfo
deserializeKeyInfo e@(CElem _ _) = do
    -- FIXME throw out incomplete keys?
    keyInfos <- sequence $ map deserializeKeyInfoColumn columnNodes

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
    
    attributeInfo <- sequence $ map deserializeTableRefColumn columnNodes
    
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
deserializeEmptyTable :: [Content i] -> Maybe PFAlgebra
deserializeEmptyTable contents = do
    -- FIXME sequence maybe to strict here ? try mapMaybe otherwise
    schema <- sequence $ map process contents
    return $ NullaryOp $ EmptyTable schema
    
  where process (CElem e _) = Just . fst =<< deserializeEmptyTableColumn e
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
    "true"  -> Just True
    "false" -> Just False
    _       -> Nothing


-- deserialize an AVal with a given ATy
deserializeAVal :: ATy -> String -> Maybe AVal
deserializeAVal t s = case t of
    AInt    -> Just . VInt =<< readMaybe s
    AStr    -> Just . VStr =<< readMaybe s
    ABool   -> Just . VBool =<< readMaybe s
    ADec    -> Just . VDec =<< readMaybe s
    ADouble -> Just . VDouble =<< readMaybe s
    ANat    -> Just . VNat =<< readMaybe s
    -- FIXME not used because no value type available?
    ASur    -> Nothing

-- deserialize a ATy from a string (from XML version)
-- FIXME there should really be an instance of Read ATy ...
deserializeATy :: String -> Maybe ATy
deserializeATy s = case s of
    "int"  -> Just AInt
    "str"  -> Just AStr
    "bool" -> Just ABool
    "dec"  -> Just ADec
    "dbl"  -> Just ADouble
    "nat"  -> Just ANat
    -- FIXME "nat" is also used for ASur, another mistake ?
    _      -> Nothing

deserializeSortDir :: String -> Maybe SortDir
deserializeSortDir s = case s of
    "ascending"  -> Just Asc
    "descending" -> Just Desc
    _            -> Nothing

-- FIXME may be defined in ghc 7.6 ?
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing
    
-- FIXME defined in GHC.Exts but without Maybe
the :: Eq a => [a] -> Maybe a
the []     = Nothing
the (x:xs) = helper x xs
  where helper y []     = Just y
        helper y (z:zs) | (y == z)  = helper z zs
                        | otherwise = Nothing




