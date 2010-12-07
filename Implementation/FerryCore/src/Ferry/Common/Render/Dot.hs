{-# LANGUAGE TypeSynonymInstances #-}
{-| Infrastructure for generating Dot graphics files.
-}
module Ferry.Common.Render.Dot where
    
import Ferry.Compiler.Error.Error

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.State

import qualified Data.List as L

-- | Class for transforming values into either an error or a string representing a dot file.
class Dotify a where
  dot :: a -> Either FerryError String

-- | Dot files are internally represented as a list of nodes and a list of edges
data DotFile = DotFile [Node] [Edge]

-- | A dot Id is just string
type Id = String

-- | A dot Node has an id (unique) and a list of properties decribing its shape
data Node = Node Id [NodeProp]

-- | An edge runs from one node to one or more others identified by unique ids
data Edge = Edge Id [Id]

-- | Node properties describing shape of a node
data NodeProp = Label Label
              | Shape Shape
              | Color Color
              | TextColor Color

{- | A dot label comes in three forms:
a primitive label (just a string) SLabel
a horizontally list of labels HLabel
and a vertically ordered list of labels VLabel
-}
data Label = SLabel String
           | HLabel [Label]
           | VLabel [Label]

-- | The shape of a dot node           
data Shape = Rect
           | Circle
           | Oval
           | Triangle

-- | Colors           
data Color = Red
           | Blue
           | Green
           | Yellow
           | Black
           | White
           | Gray

{- | Dot monad
While generating a dot file it is most convenient to do this in a monadic environment.
The inner state monad contains a supply that is used to generate unique identifiers.
In the inner Writer monad we store all the edges, the second writer monad contains
all the nodes. The error monad is used to register any eventual problems, while
preserving the state of the inner monads.
-}              
type Dot = ErrorT FerryError (WriterT [Node] (WriterT [Edge] (State Int)))

-- | Generate a new node with the given nodeproperties, returns the new
-- fresh id in the Dot environment
node :: [NodeProp] -> Dot Id
node props = do
                    i <- getFreshId
                    addNode $ Node i props
                    return i

-- | Generate an edge from arg1 to the nodes in arg2 and register it in the dot environment.
edge :: Id -> [Id] -> Dot ()
edge i is = addEdge $ Edge i is

-- | Given a dot environment generate either the error that the computation in the environment yields
-- or the resulting dot file as a string.
runDot :: Dot a -> Either FerryError String
runDot d = case r of
            Left err -> Left err
            Right _  -> Right $ dotFile ns es 
 where (((r, ns), es), _) = flip runState 0 $ runWriterT $ runWriterT $ runErrorT d

-- | Given a list of nodes and a list of edges generate a dot graph
dotFile :: [Node] -> [Edge] -> String
dotFile ns es = "digraph g {\nordering=out;" ++ concatMap dotNode ns ++ concatMap dotEdge es ++ "}"

-- | Generate the line that describes an edge in a dot file
dotEdge :: Edge -> String
dotEdge (Edge i ts) = concat [i ++ " -> " ++ t ++ ";\n" | t <- ts]

-- | Generate the line that describes a node in a dot file
dotNode :: Node -> String
dotNode (Node i props) = i ++ "[" ++ (concat $ L.intersperse "," $ map propsDot props) ++"];\n"

-- | Transform the properties into their dot representation
propsDot :: NodeProp -> String
propsDot (Shape Rect)     = "shape=record" 
propsDot (Shape Circle)   = "shape=circle"
propsDot (Shape Oval)     = "shape=ellipse"
propsDot (Shape Triangle) = "shape=triangle"
propsDot (Color Red)      = "fillcolor=red,style=filled"
propsDot (Color Blue)     = "fillcolor=blue,style=filled"
propsDot (Color Green)    = "fillcolor=green,style=filled"
propsDot (Color Yellow)   = "fillcolor=yellow,style=filled"
propsDot (Color Black)    = "fillcolor=black,style=filled"
propsDot (Color White)    = "fillcolor=white,style=filled"
propsDot (Color Gray)     = "fillcolor=gray,style=filled"
propsDot (TextColor Red)      = "color=red"
propsDot (TextColor Blue)     = "color=blue"
propsDot (TextColor Green)    = "color=green"
propsDot (TextColor Yellow)   = "color=yellow"
propsDot (TextColor Black)    = "color=black"
propsDot (TextColor White)    = "color=white"
propsDot (TextColor Gray)     = "color=gray"
propsDot (Label l)            = "label=\"" ++ labelDot l ++ "\""

-- | Transform a label into its dot representation
labelDot :: Label -> String
labelDot (SLabel s) = escape s 
labelDot (HLabel ls) = concat $ L.intersperse " | " $ map labelDot ls
labelDot (VLabel ls) = "{" ++ (concat $ L.intersperse " | " $ map (\l -> "{" ++ labelDot l ++ "}") ls) ++"}"

-- | Add an edge to the dot environment
addEdge :: Edge -> Dot ()
addEdge e = lift $ lift $ tell [e]

-- | Add a node to the dot environment
addNode :: Node -> Dot ()
addNode n = tell [n]

-- | Generate a fresh identifier
getFreshId :: Dot Id
getFreshId = do
              n <- get
              put $ n + 1
              return $ (:) 'n' $ show n

-- | Escape certain characters in a dot file              
escape :: String -> String
escape (x:xs) = case x of
                  '{' -> "\\{"
                  '}' -> "\\}"
                  '>' -> "\\>"
                  '<' -> "\\<"
                  _ -> [x]
                 ++ escape xs
escape []     = []
