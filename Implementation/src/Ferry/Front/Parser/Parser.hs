module Ferry.Front.Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Ferry.Front.Parser.Scanner
import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta

type FParser a = Parser (a, SourcePos)

parseFerry :: SourceName -> [Char] -> Either ParseError (Expr, SourcePos)
parseFerry file src = parse opExpr file src

{-
 Parsing expressions
-}

expr :: FParser Expr
expr = opExpr

opExpr :: FParser Expr
opExpr = buildExpressionParser operators simpleExpr
  where
    operators =
        [ [ unop "not"]
        , [ binop "*"  AssocLeft, binop "/"  AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        , [ binop "%"  AssocLeft, binop "contains"  AssocLeft]
        , [ binop "==" AssocNone, binop "!=" AssocNone, binop "<="  AssocNone
          , binop "<" AssocNone, binop ">="  AssocNone, binop ">" AssocNone ]
        , [ binop "and" AssocRight ] -- Right for shortcircuiting
        , [ binop "or" AssocRight ] -- Right for shortcircuiting
        , [ binop "^" AssocLeft ]
        ]
        where
          binop name assoc   = flip Infix assoc $ do
                                                    pos <- getPosition
                                                    reservedOp name
                                                    return (\(e1, p) (e2, _) -> (BinOp (Meta p) (Op (Meta pos) name) e1 e2, p)) 
          unop name     = Prefix  $ do
                                     pos <- getPosition
                                     reservedOp name
                                     return (\(e, _) -> (UnOp (Meta pos) (Op (Meta pos) name) e, pos))
                                      
simpleExpr :: FParser Expr
simpleExpr = choice [ ifExpr,
                      letExpr,
                      app
                    ]

pattern :: FParser Pattern
pattern = choice [
                  do
                   pos <- getPosition
                   v <- identifier
                   return $ (PVar (Meta pos) v, pos),
                  do
                   pos <- getPosition
                   vars <- parens (commaSep1 identifier)
                   return $ (PPat (Meta pos) vars, pos)
                 ]
{-
abstract :: FParser Expr
abstract = parens $ do 
             Abstr 
             -}
ifExpr :: FParser Expr
ifExpr = do
            pos <- getPosition
            reserved "if"
            (e1, _) <- expr
            reserved "then"
            (e2, _) <- expr
            reserved "else"
            (e3, _) <- expr
            return $ (If (Meta pos) e1 e2 e3, pos)
            
letExpr :: FParser Expr
letExpr = do
            pos <- getPosition
            reserved "let"
            bs <- commaSep1 binding
            reserved "in"
            (e,_) <- expr
            return (Let (Meta pos) (map fst bs) e, pos)
            
binding :: FParser Binding
binding = do
            pos <- getPosition
            x <- identifier
            symbol "="
            (e, _) <- expr
            return (Binding (Meta pos) x e, pos)
            
app :: FParser Expr
app = chainl1 atom (return (\(e1, p) (e2, _) -> (App (Meta p) e1 e2, p)))

atom :: FParser Expr
atom = choice [ constParser, parenExpr, variable]

parenExpr :: FParser Expr
parenExpr = do
              pos <- getPosition
              (e, _) <- parens expr
              return (Paren (Meta pos) e, pos)
              
variable :: FParser Expr
variable = do
            pos <- getPosition
            x <- identifier
            return (Var (Meta pos) x, pos)
            

{-
 Parsing primitive values
-}

constParser :: FParser Expr
constParser = do  
               (c, p) <- choice [
                                try floatParser,
                                try intParser,
                                try stringParser,
                                try boolParser                      
                                ]
               return (Const (Meta p) c, p) 

intParser :: FParser Const
intParser = do 
             pos <- getPosition
             v <- integer
             return (CInt v, pos)
             
floatParser :: FParser Const
floatParser = do
                pos <- getPosition
                f <- float
                return (CFloat f, pos)
                
stringParser :: FParser Const
stringParser = do
                 pos <- getPosition
                 symbol "\""
                 s <- many (noneOf "\"")
                 symbol "\""
                 return (CString s, pos)
                 
boolParser :: FParser Const
boolParser = try trueParser
                <|> falseParser
             
trueParser :: FParser Const
trueParser = do
               pos <- getPosition
               reserved "True"
               return (CBool True, pos)

falseParser :: FParser Const
falseParser = do
                pos <- getPosition
                reserved "False"
                return (CBool False, pos)