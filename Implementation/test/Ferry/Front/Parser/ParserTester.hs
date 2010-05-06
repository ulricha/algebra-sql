module Ferry.Front.Parser.ParserTester (parserTests) where
    
import Test.HUnit

import Network.URI (unEscapeString)

import Ferry.Front.Parser.TestDataTypes

import Ferry.Front.Parser.Parser
import Ferry.Front.Data.Language


import Text.ParserCombinators.Parsec (parse, getPosition, (<|>), try, 
                                      noneOf, many, SourcePos(..), 
                                      Parser(..), choice, chainl1,
                                      ParseError(..),SourceName(..),
                                      option, eof)
                                      
import Ferry.Front.Parser.PrimTest1

parserTester :: String -> Int -> [ParserTest] -> [Test]
parserTester n i (p:ps) = (parserTest (n ++ (show i)) p) : (parserTester n (i+1) ps)
parserTester _ _ []     = [] 

parserTest :: String -> ParserTest -> Test
parserTest n (p, e) = TestLabel n $ TestCase (assertEqual ("for " ++ p) e (case parseFerry "test" p of
                                                                                (Left e)  -> Left $ unEscapeString (show e)
                                                                                (Right e) -> Right e))
                                                                                
parserTests :: Test                                                                                
parserTests = TestList [TestLabel "primitives" $ TestList $ parserTester "primitives" 1 primTests]
                  