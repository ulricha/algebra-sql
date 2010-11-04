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
                                        , Right (QComp (Meta $ newPos "test" 1 1) (FerryCompr (Meta $ newPos "test" 1 1) [(PPat (Meta $ newPos "test" 1 5) ["x1","x2"],List (Meta $ newPos "test" 1 17) [Record (Meta $ newPos "test" 1 18) [TuplRec (Meta $ newPos "test" 1 19) 1 (Const (Meta $ newPos "test" 1 19) (CInt 1)),TuplRec (Meta $ newPos "test" 1 21) 2 (Const (Meta $ newPos "test" 1 21) (CInt 2))]])] [] (Return (Meta $ newPos "test" 1 25) (List (Meta $ newPos "test" 1 32) [Var (Meta $ newPos "test" 1 33) "x1",Var (Meta $ newPos "test" 1 37) "x2"]) Nothing))))
           ]



