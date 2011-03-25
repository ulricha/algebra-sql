module Database.Ferry.Common.Data.Plans where

import Database.Ferry.Algebra(Columns, AlgNode)
import qualified Data.Map as M

import Text.PrettyPrint.HughesPJ

import Database.Ferry.Algebra(AlgPlan, Column(..))
import Database.Ferry.Algebra.Render.XML
    
newtype SubPlan = SubPlan (M.Map Int AlgRes)

instance Show SubPlan where
    show (SubPlan p) = "SubPlans " ++ (show $ map (\(_,y,z) -> show (y, z)) $ M.elems p)

emptyPlan :: SubPlan
emptyPlan = SubPlan M.empty

subPlan :: Int -> AlgRes -> SubPlan
subPlan i p = SubPlan $ M.singleton i p

getPlan :: Int -> SubPlan -> AlgRes
getPlan i (SubPlan p) = p M.! i
-- | An algebraic solution is a triple consisting of the node id, a description of the database columns and all subplans
type AlgRes = (AlgNode, Columns, SubPlan)

-- * Rendering plans

-- Transform a query plan with result type into a pretty doc.
-- The type is used to add meta information to the XML that is used for pretty printing by ferryDB
transform :: (Bool, Bool, AlgPlan AlgRes) -> Doc
transform (isList, debug, p) = let plans = runXML False M.empty M.empty $ planBuilder debug (mkProperty isList) p
                                   planBundle = mkPlanBundle plans
                                in (document $ mkXMLDocument planBundle)
                                

-- Transform a potentially nested algebraic plan into xml.
-- The first argument is the overall result type property of the query.
planBuilder :: Bool -> Element () -> AlgPlan AlgRes -> XML ()
planBuilder debug prop (nodes, (top, cols, subs), tags) = buildPlan Nothing (Just prop) (top, cols, subs)
    where
        buildPlan :: Maybe (Int, Int) -> Maybe (Element ()) -> AlgRes -> XML ()
        buildPlan parent props (top', cols', subs') = 
                                    do
                                        let colProp = cssToProp cols'
                                        let planProp = case props of
                                                        Nothing -> [colProp] `childsOf` xmlElem "properties"
                                                        Just p  -> [colProp, p] `childsOf` xmlElem "properties"
                                        let plan = runXML debug nodeTable tags $ serializeAlgebra ((:) iterCol $ (:) posCol $ fst $ colsToNodes 1 cols') top' 
                                        pId <- mkQueryPlan parent planProp plan
                                        buildSubPlans pId subs'
        buildSubPlans :: Int -> SubPlan -> XML ()
        buildSubPlans parent (SubPlan m) = let subPlans = M.toList m
                                            in mapM_ (\(cId, res) -> buildPlan (Just (parent, cId)) Nothing res) subPlans

        nodeTable = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList nodes

-- Create an xml property node so that ferryDB knows more or less how to print the result
mkProperty :: Bool -> Element ()
mkProperty isList = [attr "name" "overallResultType", attr "value" result] `attrsOf` xmlElem "property"
    where
        result = case isList of
                    True  -> "LIST"
                    False -> "TUPLE"
                            
-- Convert columns structure to xml properties for rendering by ferry DB        
cssToProp :: Columns -> Element ()
cssToProp cols = map csToProp cols `childsOf` [attr "name" "cs"] `attrsOf` xmlElem "property"

csToProp :: Column -> Element ()
csToProp (Col i ty) = [[attr "name" "type", attr "value" $ show ty] `attrsOf` xmlElem "property"] `childsOf` [attr "name" "offset", attr "value" $ show i] `attrsOf` xmlElem "property"  
csToProp (NCol x css) = [cssToProp css] `childsOf` [attr "name" "mapping", attr "value" x] `attrsOf` xmlElem "property" 

-- Transform cs structure into xml columns
colsToNodes :: Int -> Columns -> ([Element ()], Int)
colsToNodes i ((Col n _):cs) = let col = [attr "name" $ "item" ++ (show n), attr "new" "false", attr "function" "item", attr "position" $ show i] `attrsOf` xmlElem "column"
                                   (els, i') = colsToNodes (i+1) cs
                                in (col:els, i') 
colsToNodes i ((NCol _ cs):cs') = let (els, i') = colsToNodes i cs 
                                      (els', i'') = colsToNodes i' cs'
                                   in (els ++ els', i'')
colsToNodes i []                = ([], i)


