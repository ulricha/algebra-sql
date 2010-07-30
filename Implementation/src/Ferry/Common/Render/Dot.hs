{-# LANGUAGE TypeSynonymInstances #-}
module Ferry.Common.Render.Dot where
    
import Ferry.Compiler.Error.Error

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.State

import qualified Data.List as L

data DotFile = DotFile [Node] [Edge]

type Id = String

data Node = Node Id [NodeProp]

data Edge = Edge Id [Id]

data NodeProp = Label Label
              | Shape Shape
              | Color Color
              | TextColor Color

data Label = SLabel String
           | HLabel [Label]
           | VLabel [Label]
           
data Shape = Rect
           | Circle
           | Oval
           | Triangle
           
data Color = Red
           | Blue
           | Green
           | Yellow
           | Black
           | White
           | Gray
              
type Dot = ErrorT FerryError (WriterT [Node] (WriterT [Edge] (State Int)))

node :: Id -> [NodeProp] -> Dot ()
node i props = addNode $ Node i props

edge :: Id -> [Id] -> Dot ()
edge i is = addEdge $ Edge i is

runDot :: Dot a -> Either FerryError String
runDot d = case r of
            Left err -> Left err
            Right _  -> Right $ dotFile ns es 
 where (((r, ns), es), _) = flip runState 0 $ runWriterT $ runWriterT $ runErrorT d

dotFile :: [Node] -> [Edge] -> String
dotFile ns es = "digraph g {\nordering=out;" ++ concatMap dotNode ns ++ concatMap dotEdge es ++ "}"

dotEdge :: Edge -> String
dotEdge (Edge i ts) = concat [i ++ " -> " ++ t ++ ";\n" | t <- ts]


dotNode :: Node -> String
dotNode (Node i props) = i ++ "[" ++ (concat $ L.intersperse "," $ map propsDot props) ++"];\n"

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
propsDot (Label l)            = "label=\"" ++ labelDot l ++ "\""

labelDot :: Label -> String
labelDot (SLabel s) = escape s 
labelDot (HLabel ls) = concat $ L.intersperse " | " $ map labelDot ls
labelDot (VLabel ls) = "{" ++ (concat $ L.intersperse " | " $ map (\l -> "{" ++ labelDot l ++ "}") ls) ++"}"


addEdge :: Edge -> Dot ()
addEdge e = lift $ lift $ tell [e]

addNode :: Node -> Dot ()
addNode n = tell [n]

getFreshId :: Dot Id
getFreshId = do
              n <- get
              put $ n + 1
              return $ (:) 'n' $ show n
              
escape :: String -> String
escape (x:xs) = case x of
                  '{' -> "\\{"
                  '}' -> "\\}"
                  '>' -> "\\>"
                  '<' -> "\\<"
                  otherwise -> [x]
                 ++ escape xs
escape []     = []