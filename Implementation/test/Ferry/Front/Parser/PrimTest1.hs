module Ferry.Front.Parser.PrimTest1 where
    
import Test.HUnit

import Ferry.Front.Parser.TestDataTypes

import Text.ParserCombinators.Parsec.Pos

import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta

pos = newPos "test" 1 1


primTests :: [ParserTest]
primTests = [("1"                       , Right (Const (Meta pos) (CInt 1))),
             ("0"                       , Right (Const (Meta pos) (CInt 0))),
             ("123456789"               , Right (Const (Meta pos) (CInt 123456789))),
             ("-1"                      , Right (Const (Meta pos) (CInt (-1)))),
             ("-42"                     , Right (Const (Meta pos) (CInt (-42)))),
             ("-123456789"              , Right (Const (Meta pos) (CInt (-123456789)))),
             ("\"Hello World!\""        , Right (Const (Meta pos) (CString "Hello World!"))),
             ("\"+-!@33545ada.,.,1,2\"" , Right (Const (Meta pos) (CString "+-!@33545ada.,.,1,2"))),
             ("\"\""                    , Right (Const (Meta pos) (CString ""))),
             (""                        ,  Left "\"test\" (line 1, column 1):\nunexpected end of input\nexpecting \"if\", \"let\", \"relationship\", \"for\", \"(\", \"{\", \"[\", \"table\", float, natural, \"\\\"\", \"True\", \"False\" or identifier")
                  
            ]



