module Ferry.Front.Parser.Parser where

import Text.ParserCombinators.Parsec (parse, getPosition, (<|>), try, 
                                      noneOf, many, SourcePos(..), 
                                      Parser(..), choice, chainl1,
                                      ParseError(..),SourceName(..),
                                      option, eof)
import Text.ParserCombinators.Parsec.Expr

import Ferry.Front.Parser.Scanner
import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta

parseFerry :: SourceName -> [Char] -> Either ParseError Expr
parseFerry file src = parse parseInput file src

parseInput :: Parser Expr
parseInput = do
                whiteSpace
                e <- expr
                eof
                return e

{-
 Parsing expressions
-}

expr :: Parser Expr
expr = opExpr

opExpr :: Parser Expr
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
                                                    return (\e1 e2 -> BinOp (Meta $ getPos e1) (Op (Meta pos) name) e1 e2) 
          unop name     = Prefix  $ do
                                     pos <- getPosition
                                     reservedOp name
                                     return (\e -> UnOp (Meta pos) (Op (Meta pos) name) e)
                                      
simpleExpr :: Parser Expr
simpleExpr = choice [ ifExpr,
                      letExpr,
                      relationship,
                      for,
                      app
                    ]

abstract :: Parser Expr
abstract = do 
            pos <- getPosition
            a <- parens $ do 
                            pat <- pattern
                            symbol "->"
                            e <- expr
                            return (\p -> Abstr (Meta p) pat e)
            return $ a pos

tuple :: Parser Expr
tuple = do
          pos <- getPosition
          t <- parens $ do
                          e <- expr
                          comma
                          es <- commaSep1 expr
                          return (\p -> Record (Meta p) $ listToRecElem (e:es) [1..])
          return $ t pos
    where 
        listToRecElem :: [Expr] -> [Int] -> [RecElem]
        listToRecElem (e:es) (i:is) = (:) (TuplRec (Meta $ getPos e) i e) $ listToRecElem es is
        listToRecElem []     _      = []
        
record :: Parser Expr
record = do
           pos <- getPosition
           r <- braces $ commaSep1 $ do 
                                       pos <- getPosition
                                       i <- identifier
                                       symbol "="
                                       e <- expr
                                       return $ TrueRec (Meta pos) i e
           return $ Record (Meta pos) r
           
list :: Parser Expr
list = do 
        pos <- getPosition
        el <- squares $ commaSep expr
        return $ List (Meta pos) el                               
             
ifExpr :: Parser Expr
ifExpr = do
            pos <- getPosition
            reserved "if"
            e1 <- expr
            reserved "then"
            e2 <- expr
            reserved "else"
            e3 <- expr
            return $ If (Meta pos) e1 e2 e3
            
letExpr :: Parser Expr
letExpr = do
            pos <- getPosition
            reserved "let"
            bs <- commaSep1 binding
            reserved "in"
            e <- expr
            return $ Let (Meta pos) bs e
            
table :: Parser Expr
table = do 
          pos <- getPosition
          reserved "table"
          n <- tableName
          cs <- parens $ commaSep1 column
          reserved "with"
          reserved "keys"
          keys <- parens $ commaSep1 key
          return $ Table (Meta pos) n cs keys
          
relationship :: Parser Expr
relationship = do 
                pos <- getPosition
                reserved "relationship"
                reserved "from"
                c1 <- cardinality
                e1 <- expr
                reserved "to"
                c2 <- cardinality
                e2 <- expr
                reserved "by"
                k1 <- key
                reserved "eq"
                k2 <- key
                return $ Relationship (Meta pos) c1 e1 c2 e2 k1 k2
                
for :: Parser Expr
for = do 
        pos <- getPosition
        reserved "for"
        p <- pattern
        reserved "in"
        e <- expr
        l1 <- many forLet
        l2 <- option [] forWhere
        l3 <- many forLet
        l4 <- option [] forGroup
        l5 <- many forLet
        l6 <- option [] forWhere
        l7 <- many forLet
        l8 <- option [] forOrder
        l9 <- many forLet
        reserved "return"
        er <- expr
        return $ For (Meta pos) p e (concat [l1, l2, l3, l4, l5, l6, l7, l8, l9]) er
        
forLet :: Parser ForElem
forLet = do
            pos <- getPosition
            reserved "let"
            p <- pattern
            symbol "="
            e <- expr
            return $ FLet (Meta pos) p e
            
forWhere :: Parser [ForElem]
forWhere = do
            pos <- getPosition
            reserved "where"
            e <- expr
            return $ [FWhere (Meta pos) e]
            
forGroup :: Parser [ForElem]
forGroup = do
            pos <- getPosition
            reserved "group"
            reserved "by"
            groups <- commaSep1 expr
            return $ [FGroup (Meta pos) groups]
            
forOrder :: Parser [ForElem]
forOrder = do
            pos <- getPosition
            reserved "order"
            reserved "by"
            ords <- commaSep1 elem
            return $ [FOrder (Meta pos) ords]
    where
        elem :: Parser ExprOrder
        elem = do 
                 pos <- getPosition
                 e <- expr
                 o <- option (Ascending $ Meta emptyPos) ordering
                 return $ ExprOrder (Meta pos) e o
            
app :: Parser Expr
app = chainl1 atom (return (\e1 e2 -> App (Meta $ getPos e1) e1 e2))

atom :: Parser Expr
atom = do 
        e <- choice [ try abstract, 
                      try tuple,
                      record,
                      list,
                      table,
                      constParser, 
                      parenExpr, 
                      variable]
        el <- many element
        return $ case el of
                  [] -> e
                  _  -> foldl (\l r -> Elem (Meta $ getPos e) l r) e el  

parenExpr :: Parser Expr
parenExpr = do
              pos <- getPosition
              e <- parens expr
              return $ Paren (Meta pos) e
              
variable :: Parser Expr
variable = do
            pos <- getPosition
            x <- identifier
            return $ Var (Meta pos) x
            
element :: Parser (Either String Integer)
element = do 
            symbol "."
            i <- choice [
                          try $ do 
                                  n <- natural
                                  return $ Right n
                        , try $ do
                                  i <- identifier
                                  return $ Left i
                        ]
            return i

tableName :: Parser String
tableName = choice [try qualifiedTableName, simpleTableName]

qualifiedTableName :: Parser String
qualifiedTableName = do
                 schema <- identifier
                 symbol "."
                 name <- identifier
                 return $ schema ++ "." ++ name
                 
simpleTableName :: Parser String
simpleTableName = do 
                    name <- identifier
                    return name
        

binding :: Parser Binding
binding = do
            pos <- getPosition
            x <- identifier
            symbol "="
            e <- expr
            return $ Binding (Meta pos) x e

-- | Parse a pattern
--
-- A pattern is either a single identifier x, or a serie of identifiers (x1, ..., xn)            
pattern :: Parser Pattern
pattern = choice [
                  do
                   pos <- getPosition
                   v <- identifier
                   return $ PVar (Meta pos) v,
                  do
                   pos <- getPosition
                   vars <- parens (commaSep1 identifier)
                   return $ PPat (Meta pos) vars
                 ]

primType :: Parser Type
primType = choice [typeParser n c | (n, c) <- types] 
    where typeParser n c = do
                            pos <- getPosition
                            reserved n
                            return $ c (Meta pos)
          types = [("String", TString), ("Bool", TBool), ("Int", TInt), ("Float", TFloat)]
          
column :: Parser Column
column = do
            pos <- getPosition
            n <- identifier
            t <- primType
            return $ Column (Meta pos) n t

key :: Parser Key
key = do
        pos <- getPosition
        cs <- parens $ commaSep1 identifier
        return $ Key (Meta pos) cs 
                 
-- | Parse cardinality
--
-- Cardinality is used in relationships and can either be one or many
cardinality :: Parser Cardinality
cardinality = choice [cOne, cMany]

cOne :: Parser Cardinality
cOne = do
        pos <- getPosition
        reserved "one"
        return $ One $ Meta pos
        
cMany :: Parser Cardinality
cMany = do
         pos <- getPosition
         reserved "many"
         return $ Many $ Meta pos

-- | Parse ordening
--
-- An ordering can be used in the order by clause in a list comprehension                 
ordering :: Parser Order
ordering = choice [ascending, descending]

ascending :: Parser Order
ascending = do 
              pos <- getPosition
              reserved "ascending"
              return $ Ascending $ Meta pos
              
descending :: Parser Order
descending = do
               pos <- getPosition
               reserved "descending"
               return $ Descending $ Meta pos
                
{-
 Parsing primitive values
-}

constParser :: Parser Expr
constParser = do  
               (c, p) <- choice [
                                try floatParser,
                                try intParser,
                                try stringParser,
                                try boolParser                      
                                ]
               return $ Const (Meta p) c 

type FParser a = Parser (a, SourcePos)

intParser :: FParser Const
intParser = do 
             pos <- getPosition
             v <- natural
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