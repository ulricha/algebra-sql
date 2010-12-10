module Ferry.Algebra.Render.XMLUtils where
    
import Text.XML.HaXml.Types
import Text.XML.HaXml.Escape (mkXmlEscaper, XmlEscaper ())

import Ferry.Algebra.Data.Algebra

import qualified Data.Map as M
import Data.Char (ord)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

-- Convenient alias for column names
type ColName = String

-- The Graph is represented as a tuple of an int, that represents the first node, and
-- a list of algebraic nodes with their node numbers.
type Graph = (AlgNode, [(Algebra, AlgNode)])

-- Alias for GraphNode ids
type GraphNode = Int
-- Alias for xmlNode ids
type XMLNode = Int

-- Mapping from graphnodes to xmlnode ids. This dictionary is used to prevent duplicate xml nodes
type Dictionary = M.Map GraphNode XMLNode

-- XML monad, all elements are printed in bottom up!!! order into the writer monad so
-- that the xml can easily be printed an will be accepted by pfopt.
-- The reader monad contains the map with all the nodes from the algebraic plan, the keys
-- are the node ids from the graph. The state monad keeps track of the supply of fresh ids
-- for xml nodes and the dictionary for looking up whether a certain graphnode already has
-- an xml representation.
type XML = WriterT [Element ()] (ReaderT (M.Map AlgNode Algebra) (State (Int, Dictionary)))

-- Has a graphnode already been translated into an xml node. If yes which node?
isDefined :: GraphNode -> XML (Maybe XMLNode)
isDefined g = do
                (_, d) <- get
                return $ M.lookup g d 

-- Get a fresh xml node id.
freshId :: XML Int
freshId = do
            (n, d) <- get
            put (n + 1, d)
            return n

-- Add a mapping from a graphnode to an xml node id to the dictionary            
addNodeTrans :: GraphNode -> XMLNode -> XML ()
addNodeTrans gId xId = do
                        (n, d) <- get
                        put (n, M.insert gId xId d)

-- Get a node from the algebraic plan with a certain graphNode id number
getNode :: Int -> XML Algebra
getNode i = do
             nodes <- ask
             return $ nodes M.! i


-- Run the monad and return a list of xml elements from the monad.
runXML :: M.Map AlgNode Algebra -> XML a -> [Element ()]
runXML m = snd . fst . flip runState (0, M.empty) . flip runReaderT m . runWriterT

xmlEscaper :: XmlEscaper
xmlEscaper = mkXmlEscaper
   [('\60',"lt"),('\62',"gt"),('\38',"amp"),('\39',"apos"),('\34',"quot"), ('\92', "\\")]
   (\ ch ->
      let
         i = ord ch
      in
         i < 10 || (10<i && i<32) || i >= 127 ||
            case ch of
               '\'' -> True
               '\"' -> True
               '&' -> True
               '<' -> True
               '>' -> True
               '\\' -> True 
               _ -> False
      )

infixr 0 `childsOf`
infixr 0 `dataChildOf`
infixr 0 `attrsOf`

childsOf :: [Element ()] -> Element () -> Element () 
childsOf cs (Elem n attrs cs') = Elem n attrs $ cs' ++ [CElem c () | c <- cs]

dataChildOf :: Show a => a -> Element () -> Element ()
dataChildOf v (Elem n attrs cs) = Elem n attrs $ (CString False (show v) ()) : cs

column :: String -> Bool -> Element ()
column n v = let new = case v of
                        True -> "true"
                        False -> "false"
              in [attr "name" n, attr "new" new] `attrsOf` xmlElem "column"
              
typeN :: ATy -> Element ()
typeN t = [attr "name" $ show t] `attrsOf` xmlElem "type"

xmlElem :: String -> Element ()
xmlElem n = Elem n [] []

node :: XMLNode -> String -> Element ()
node xId t = [attr "id" $ show xId, attr "kind" t] `attrsOf` xmlElem "node"

contentNode :: Element ()
contentNode = xmlElem "content"

attr :: String -> String -> Attribute
attr n v = (n, AttValue [Left v])

attrsOf :: [Attribute] -> Element () -> Element ()
attrsOf at (Elem n attrs cs) = Elem n (at ++ attrs) cs