module Ferry.Front.Parser.Parser where


import Text.ParserCombinators.Parsec.Expr

import Ferry.Front.Parser.Applicative hiding (Const, Column)
import Ferry.Front.Parser.Scanner
import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Instances
import Ferry.Front.Data.Base

parseFerry :: SourceName -> [Char] -> Either ParseError Expr
parseFerry file src = parse parseInput file src

parseInput :: Parser Expr
parseInput = whiteSpace *> expr <* eof

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
simpleExpr = ifExpr <|> letExpr <|> relationship <|>  try app <|>  queryComprehension <|>  atom

-- | Parser for if then else contructs. The expression contained in the conditional and branches are
--   regular top level expression 'expr'.
ifExpr :: Parser Expr
ifExpr = If <$> pMeta <* reserved "if" <*> expr <* reserved "then" <*> expr <* reserved "else" <*> expr

-- | Parser for let bindings.
letExpr :: Parser Expr
letExpr = Let <$> pMeta <* reserved "let" <*> commaSep1 binding <* reserved "in" <*> expr

-- | Parser for relationship contruct.
relationship :: Parser Expr
relationship = Relationship <$> pMeta <* reserved "relationship" <* reserved "from" <*> cardinality
                <*> expr <* reserved "to" <*> cardinality <*> expr <* reserved "by" 
                <*> key <* reserved "eq" <*> key

arg :: Parser Arg
arg = AExpr <$> pMeta <*> atom
        <|> abstract

-- | Parse function abstraction
abstract :: Parser Arg
abstract = (\m (p, e) -> AAbstr m p e) <$> pMeta <*> parens ((\p e -> (p, e)) <$> commaSep1 pattern <* symbol "->" <*> expr) 

-- | Parser for function application. Parse an atomic expression followed by as much
--   atomic expressions as possible.If there is no application then parse at least
--   one atomic expression 'atom'.
app :: Parser Expr
app = App <$> pMeta <*> variable <*> many1 arg

queryComprehension :: Parser Expr
queryComprehension = QComp <$> pMeta <*> ferryCompr

-- Parse query comprehensions

-- | Parser for Ferry list comprehensions.
ferryCompr :: Parser QCompr
ferryCompr = (\m (For _ ps) b r -> FerryCompr m ps b r) <$> pMeta <*> forClause <*> many bodyClause <*> returnClause

forClause :: Parser BodyElem
forClause = For <$> pMeta <* reserved "for" <*> commaSep1 ((,) <$> pattern <* reserved "in" <*> expr)

bodyClause :: Parser BodyElem
bodyClause =  forClause <|> whereClause <|> letClause <|> orderByClause <|> groupClause

letClause :: Parser BodyElem
letClause = ForLet <$> pMeta <* reserved "let" <*> commaSep1 ((,) <$> pattern <* symbol "=" <*> expr)

whereClause :: Parser BodyElem
whereClause = ForWhere <$> pMeta <* reserved "where" <*> expr

orderByClause :: Parser BodyElem
orderByClause = ForOrder <$> pMeta <* reserved "order" <* reserved "by" <*> commaSep1 elem
        where
            elem :: Parser ExprOrder
            elem = ExprOrder <$> pMeta <*> expr <*> option (Ascending $ Meta emptyPos) ordering

groupClause :: Parser BodyElem
groupClause = try groupBy <|> try groupWith

groupBy :: Parser BodyElem
groupBy = (\m -> Group m GBy) <$> pMeta <* reserved "group" <*> option Nothing (Just <$> expr) 
            <* reserved "by" <*> commaSep1 expr <*> option Nothing (Just <$ reserved "into" <*> pattern)

groupWith :: Parser BodyElem
groupWith = (\m ->Group m GWith) <$> pMeta <* reserved "group" <*> option Nothing (Just <$> expr) <* reserved "with"
             <*> commaSep1 expr <*> option Nothing (Just <$ reserved "into" <*> pattern)

returnClause :: Parser ReturnElem
returnClause = Return <$> pMeta <* reserved "return" <*> expr
                <*> option Nothing ((\p bs r -> Just (p, bs, r)) <$ reserved "into" <*> pattern <*> many bodyClause <*> returnClause )

-- | Parser atomic values.
atom :: Parser Expr
atom = process <$> (try tuple <|> record <|> list <|> table <|> constParser <|> parenExpr <|> variable) <*> many lookupListRec
     where process e el = case el of
                           [] -> e
                           _  -> foldl (\l r -> case r of
                                                 Left e' -> Elem (Meta $ getPos e) l e'
                                                 Right e' -> Lookup (Meta $ getPos e) l e') e el 

lookupListRec :: Parser (Either (Either String Integer) Expr)
lookupListRec = Left <$> element <|> Right <$> listLookup

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
           r <- braces $ commaSep1 recElem
           return $ Record (Meta pos) r

recElem :: Parser RecElem
recElem = choice [try $ do
                         pos <- getPosition
                         i <- identifier
                         symbol "="
                         e <- expr
                         return $ TrueRec (Meta pos) (Right i) $ Just e,
                  try $ do
                         pos <- getPosition
                         i <- identifier
                         return $ TrueRec (Meta pos) (Right i) Nothing,
                  try $ do
                         pos <- getPosition
                         e <- atom
                         return $ TrueRec (Meta pos) (Left e) Nothing                  
                    ]
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
