module Ferry.Front.Parser.ConstructTest1 where
    
import Test.HUnit

import Ferry.Front.Parser.TestDataTypes

import Text.ParserCombinators.Parsec.Pos

import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base

pos = newPos "test" 1 1


constructTests :: [ParserTest]
constructTests = [("x"                       , Right (Var (Meta pos) "x")),
                  ("hello123"                , Right (Var (Meta pos) "hello123")),
                  ("x_y"                     , Right (Var (Meta pos) "x_y")),
                  ("x y"                     , Right (App (Meta pos) (Var (Meta pos) "x") [(AExpr (Meta $ newPos "test" 1 3) $ Var (Meta $ newPos "test" 1 3) "y")])),
                  ("x y z"                   , Left "bla"),
                  ("if True then True else False",
                                               Right (If (Meta pos) (Const (Meta $ newPos "test" 1 4) $ CBool True)
                                                                    (Const (Meta $ newPos "test" 1 14) $ CBool True)
                                                                    (Const (Meta $ newPos "test" 1 24) $ CBool False))),
                  ("1 + 1"                   , Right (BinOp (Meta pos) (Op (Meta $ newPos "test" 1 3) "+")
                                                                       (Const (Meta pos) $ CInt 1) 
                                                                       (Const (Meta $ newPos "test" 1 5) $ CInt 1))),
                  ("(1,True, \"Hello\", x y)", Left "bla"),
                  ("z (x y)"                 , Left "bla"),
                  ("{first = True, second = False}",
                                               Left "bla"),
                  ("[]"                      , Right (List (Meta pos) [])),
                  ("[1,2,3,4]"               , Right (List (Meta pos) [Const (Meta $ newPos "test" 1 2) $ CInt 1,
                                                                       Const (Meta $ newPos "test" 1 4) $ CInt 2,
                                                                       Const (Meta $ newPos "test" 1 6) $ CInt 3,
                                                                       Const (Meta $ newPos "test" 1 8) $ CInt 4])),
                  ("(1,2).2"                 , Right (Elem (Meta pos) 
                                                            (Record (Meta pos) 
                                                            [TuplRec (Meta $ newPos "test" 1 2) 1 (Const (Meta $ newPos "test" 1 2) (CInt 1)),
                                                             TuplRec (Meta $ newPos "test" 1 4) 2 (Const (Meta $ newPos "test" 1 4) (CInt 2))]) 
                                                            (Right 2))),
                  ("{first = 1}.first"       , Left "bla"),
                  ("x.first.second"          , Right (Elem (Meta pos) (Elem (Meta pos) (Var (Meta pos) "x") (Left "first")) (Left "second"))),
                  ("let x = 1 in x + 1"      , Right (Let (Meta pos) [Binding (Meta $ newPos "test" 1 5) "x" (Const (Meta $ newPos "test" 1 9) (CInt 1))] 
                                                        (BinOp (Meta $ newPos "test" 1 14) (Op (Meta $ newPos "test" 1 16) "+") (Var (Meta $ newPos "test" 1 14) "x") 
                                                                                                                                (Const (Meta $ newPos "test" 1 18) (CInt 1))))),
                  ("table db.Customer (id Int, name String) with keys ((id))",
                                               Right (Table (Meta pos) "db.Customer" 
                                                        [Column (Meta $ newPos "test" 1 20) "id" (TInt (Meta $ newPos "test" 1 23)),
                                                         Column (Meta $ newPos "test" 1 28) "name" (TString (Meta $ newPos "test" 1 33))] 
                                                        [Key (Meta $ newPos "test" 1 52) ["id"]])),
                  ("relationship from one customer to many orders by (id) eq (cId)",
                                               Right (Relationship (Meta pos) 
                                                        (One  (Meta $ newPos "test" 1 19)) (Var (Meta $ newPos "test" 1 23) "customer") 
                                                        (Many (Meta $ newPos "test" 1 35)) (Var (Meta $ newPos "test" 1 40) "orders") 
                                                        (Key (Meta $ newPos "test" 1 50) ["id"]) 
                                                        (Key (Meta $ newPos "test" 1 58) ["cId"])))                         
                 ]



