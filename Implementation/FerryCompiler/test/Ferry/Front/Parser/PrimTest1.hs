module Ferry.Front.Parser.PrimTest1 where
    
import Test.HUnit

import Ferry.Front.Parser.TestDataTypes

import Text.ParserCombinators.Parsec.Pos

import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base

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
             (""                        , Left "\"test\" (line 1, column 1):\nunexpected end of input\nexpecting \"if\", \"let\", \"relationship\", identifier, \"for\", \"(\", \"{\", \"[\", \"table\", float, \"-\", natural, \"\\\"\", \"True\", \"False\" or \"()\""),
             ("True"                    , Right (Const (Meta pos) (CBool True))),
             ("False"                   , Right (Const (Meta pos) (CBool False))),
             ("1234.5678"               , Right (Const (Meta pos) (CFloat 1234.5678))),
             ("1.0"                     , Right (Const (Meta pos) (CFloat 1.0))),
             ("0.0"                     , Right (Const (Meta pos) (CFloat 0.0))),
             ("-1.0"                    , Right (Const (Meta pos) (CFloat (-1.0)))),
             ("-123456.789"             , Right (Const (Meta pos) (CFloat (-123456.789))))
            ]



