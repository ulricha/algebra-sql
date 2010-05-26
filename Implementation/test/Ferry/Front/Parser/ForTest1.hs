module Ferry.Front.Parser.ForTest1 where
    
import Test.HUnit

import Ferry.Front.Parser.TestDataTypes

import Text.ParserCombinators.Parsec.Pos

import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base

pos = newPos "test" 1 1


forTests :: [ParserTest]
forTests = [ ("for (x1, x2) in [(1,2)] return [x1, x2]"
                                        ,  Left "bla")
           ]



