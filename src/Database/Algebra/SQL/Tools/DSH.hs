{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

import qualified Prelude as P
import Database.DSH
import qualified Database.DSH.Compiler as C
import qualified Database.DSH.Interpreter as I
import Database.HDBC.PostgreSQL
import Database.Algebra.Pathfinder.Render.JSON
import Database.Algebra.Pathfinder.Render.Dot
import qualified Data.List as L (isPrefixOf)
import System.Directory (getDirectoryContents, removeFile)
import System.FilePath (takeExtension, takeFileName)

import Database.Algebra.Dag

import File (renderDot, outputDot, readDagFromFile, optimizeXML)
import qualified Materialization.CTE as CTE
import qualified Materialization.Combined as Combined
import Util (putShowSLn, renderDebugOutput)
import Tile

connect :: IO Connection
connect = connectPostgreSQL "user='postgres' password='postgres' host='localhost'"

xs :: Q [Integer]
xs = toQ [1, 2, 3, 4, 5, 6, 7]

ys :: Q [Integer]
ys = toQ [2, 4, 6]

q :: Q [Integer]
q = [ x | x <- xs, x `elem` ys ]

foo :: Q [(Text, Integer)]
foo = tableDB "foo"

foo2:: Q [(Text, Integer, Integer)]
foo2 = [ tuple3 (fst x) (snd x) z | x <- foo, z <- toQ [0, 1] ]

bar :: Q [(Integer, Integer)]
bar = [ tuple2 x y | x <- toQ [0], y <- toQ [0, 1]]

p0 :: Q ()
p0 = unit

p1 :: Q Bool
p1 = (false && true) || not false

p2 :: Q [(Integer, Integer)]
p2 =  [ tuple2 x y
      | x <- toQ [1..10]
      , y <- toQ [1..10]
      , x == y + 1
      ]

-- select foo.bla, count(foo.id)
-- from foo as foo(bla, id)
-- group by foo.bla
p3 :: Q [(Text, Integer)]
p3 = [ tuple2 bla $ length $ map snd foos
     | (view -> (bla, foos)) <- groupWithKey fst foo
     ]

p4 :: Q [Integer]
p4 = map length $ groupWith fst foo

query = p3

main :: IO ()
main = connect
       P.>>= \c -> C.debugTA "/tmp/test" c query
       P.>> disconnect c
       P.>> getDirectoryContents "/tmp/"
       P.>>= \cs ->
       let matching   = P.filter f cs
           f filepath = L.isPrefixOf "test_" filename P.&& takeExtension filename P.== ".plan"
                      where filename = takeFileName filepath
       in case matching of
              []       -> error "no file found"
              (file:_) -> 
                  P.putStrLn ("choices were " P.++ show matching P.++ ", chose: " P.++ show file)
                  --P.>> optimizeXML ("/tmp/" P.++ file) "/tmp/formatted.xml" "/tmp/test.xml"
                  P.>> readDagFromFile ("/tmp/" P.++ file)
                  P.>>= \result ->
                  --mapM_ (removeFile . (P.++) "/tmp/") matching
                  --P.>>
                  case result of
                      Left e    -> putStrLn e
                      Right dag ->
                          putShowSLn (renderDebugOutput dag Combined.materialize True)
                          P.>> outputDot "/tmp/test.dot" dag
                          P.>> renderDot "/tmp/test.dot" "/tmp/test.png"

