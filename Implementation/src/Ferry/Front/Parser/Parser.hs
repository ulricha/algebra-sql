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
import Ferry.Front.Data.Instances
import Ferry.Front.Data.Base

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

-- | The highest level of bindings or operator bindings. The parser created by the builder will parse as many
--   parts of an arithmatic, logical expression as possible. If there is no expression it will parse at least
--   one 'simpleExpr'.
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

-- | A simpleExpr is an if, let, relationship, list comprehension or application.                                      
simpleExpr :: Parser Expr
simpleExpr = choice [ ifExpr,
                      letExpr,
                      relationship,
                      app,
                      queryComprehension                      
                    ]
                        
-- | Parser for if then else contructs. The expression contained in the conditional and branches are
--   regular top level expression 'expr'.                    
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

-- | Parser for let bindings.             
letExpr :: Parser Expr
letExpr = do
            pos <- getPosition
            reserved "let"
            bs <- commaSep1 binding
            reserved "in"
            e <- expr
            return $ Let (Meta pos) bs e

-- | Parser for relationship contruct.             
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
                
-- | Parser for function application. Parse an atomic expression followed by as much
--   atomic expressions as possible.If there is no application then parse at least
--   one atomic expression 'atom'.
app :: Parser Expr
app = chainl1 atom (return (\e1 e2 -> App (Meta $ getPos e1) e1 e2))

queryComprehension :: Parser Expr
queryComprehension = do
                        c <- choice [ferryCompr]
                        return $ QComp (Meta $ getPos c) c

-- Parse query comprehensions

-- | Parser for Ferry list comprehensions. 
ferryCompr :: Parser QCompr
ferryCompr = do 
        pos <- getPosition
        (For _ ps) <- forClause
        body <- many bodyClause
        r <- returnClause
        return $ FerryCompr (Meta pos) ps body r
 
forClause :: Parser BodyElem
forClause = do
              pos <- getPosition
              reserved "for"
              ps <- commaSep1 $ 
                    do
                        p <- pattern
                        reserved "in"
                        e <- expr
                        return (p, e)
              return $ For (Meta pos) ps 
              
bodyClause :: Parser BodyElem
bodyClause = choice [forClause,
                     whereClause,
                     letClause,
                     orderByClause,
                     groupClause]                              
        
letClause :: Parser BodyElem
letClause = do
            pos <- getPosition
            reserved "let"
            ps <- commaSep1 $
                do
                    p <- pattern
                    symbol "="
                    e <- expr
                    return (p, e)
            return $ ForLet (Meta pos) ps

whereClause :: Parser BodyElem
whereClause = do
                pos <- getPosition
                reserved "where"
                e <- expr
                return $ ForWhere (Meta pos) e

orderByClause :: Parser BodyElem
orderByClause = do
                 pos <- getPosition
                 reserved "order"
                 reserved "by"
                 os <- commaSep1 elem
                 return $ ForOrder (Meta pos) os
        where
            elem :: Parser ExprOrder
            elem = do 
                     pos <- getPosition
                     e <- expr
                     o <- option (Ascending $ Meta emptyPos) ordering
                     return $ ExprOrder (Meta pos) e o
                     
groupClause :: Parser BodyElem
groupClause = choice [try groupBy, try altGroupBy, try groupWith]
    
groupBy :: Parser BodyElem
groupBy = do
            pos <- getPosition
            reserved "group"
            reserved "by"
            e <- expr
            i <- option Nothing $ do
                                    reserved "into"
                                    p <- pattern
                                    return $ Just p
            return $ GroupBy (Meta pos) e i
            
altGroupBy :: Parser BodyElem
altGroupBy = do
                pos <- getPosition
                reserved "group"
                e <- expr
                reserved "by"
                e <- expr
                reserved "into"
                p <- pattern
                return $ AltGroupBy (Meta pos) e e p
                
groupWith :: Parser BodyElem
groupWith = do
             pos <- getPosition
             reserved "group"
             e1 <- option Nothing $ do
                                     e <- expr
                                     return $ Just e
             reserved "with"
             e2 <- expr
             i <- option Nothing $ do
                                    reserved "into"
                                    p <- pattern
                                    return $ Just p
             return $ GroupWith (Meta pos) e1 e2 i
                 
returnClause :: Parser Expr
returnClause = do
                reserved "return"
                e <- expr
                return e
                         
-- | Parser atomic values.
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
     el <- many lookupListRec
     return $ case el of
               [] -> e
               _  -> foldl (\l r -> case r of 
                                     Left e' -> Elem (Meta $ getPos e) l e'
                                     Right e' -> Lookup (Meta $ getPos e) l e') e el
               
lookupListRec :: Parser (Either (Either String Integer) Expr)
lookupListRec = choice [ do
                            e <- element
                            return $ Left e
                       , do
                            e <- listLookup
                            return $ Right e]
               
-- | Parse function abstraction                                   
abstract :: Parser Expr
abstract = do 
            pos <- getPosition
            a <- parens $ do 
                            pat <- pattern
                            symbol "->"
                            e <- expr
                            return (\p -> Abstr (Meta p) pat e)
            return $ a pos

-- | Parse a tuple. A tuple contains at least two elements.
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

-- | Parse a record.        
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

-- | Parse a list.           
list :: Parser Expr
list = do 
        pos <- getPosition
        el <- squares $ commaSep expr
        return $ List (Meta pos) el                               
             
-- | Parse a table declaration.            
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

-- | Parse constant values. A constant is either a float, integer, string or boolean value.          
constParser :: Parser Expr
constParser = do  
             (c, p) <- choice [
                              try floatParser,
                              try intParser,
                              try stringParser,
                              try boolParser                      
                              ]
             return $ Const (Meta p) c

-- | Parse a parenthesised expression. This is just an expression 'expr' surrounded by "( ... )"
parenExpr :: Parser Expr
parenExpr = do
              pos <- getPosition
              e <- parens expr
              return $ Paren (Meta pos) e

-- | Parse a variable.              
variable :: Parser Expr
variable = do
            pos <- getPosition
            x <- identifier
            return $ Var (Meta pos) x
            
listLookup :: Parser Expr
listLookup = do
              e <- braces expr
              return e
            
-- The following parser are auxiliry parsers.

-- | Parse element lookup.            
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

-- | Parse the name of a database table. Either qualified or unqualified.
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
        
-- | Parse a binding
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

-- | Parse primitive types
primType :: Parser Type
primType = choice [typeParser n c | (n, c) <- types] 
    where typeParser n c = do
                            pos <- getPosition
                            reserved n
                            return $ c (Meta pos)
          types = [("String", TString), ("Bool", TBool), ("Int", TInt), ("Float", TFloat)]

-- | Parse a database column declaration          
column :: Parser Column
column = do
            pos <- getPosition
            n <- identifier
            t <- primType
            return $ Column (Meta pos) n t

-- | Parse a table key
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


-- | Type FParser is used to parse primitive values.
type FParser a = Parser (a, SourcePos)

-- | Parse an integer, currently only positive ints are allowed
intParser :: FParser Const
intParser = do 
             pos <- getPosition
             v <- natural
             return (CInt v, pos)

-- | Parse a float, currently only positive floats are allowed             
floatParser :: FParser Const
floatParser = do
                pos <- getPosition
                f <- float
                return (CFloat f, pos)

-- | Parse a string, a string is anything surrounded by " ... "                
stringParser :: FParser Const
stringParser = do
                 pos <- getPosition
                 symbol "\""
                 s <- many (noneOf "\"")
                 symbol "\""
                 return (CString s, pos)

-- | Parse a boolean value                 
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