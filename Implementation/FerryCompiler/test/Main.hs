-- | This module runs all the unit tests on the ferry compiler source code
module Main where

import Test.HUnit

import Ferry.Front.Parser.ParserTester
    
main :: IO ()
main = do
        putStrLn "Ferry 2.0 unit tests"
        runTestTT testSet
        return ()
        
testSet :: Test
testSet = TestList [parserTests] 