module Database.Algebra.Rewrite.PatternSyntax 
       ( Pattern
       , Op(..)
       , Node(..)
       , Child(..)
       , Sem(..)
       , Ident
       , UIdent
       , parsePattern) where

import Text.ParserCombinators.Parsec

{-

S -> Op

Node    -> (Child) Op Sem (Child)
        |  Op Sem (Child)
        |  Op Sem

Op      -> Alternative
        | Id @ Alternative
        | Uid

Child -> _
      |  Op
      |  Id = Op 

Sem   -> _
      |  Id

-}

type Pattern = Node

data Node = TerP Op (Maybe Sem) Child Child Child
          | BinP Op (Maybe Sem) Child Child
          | UnP Op (Maybe Sem) Child
          | NullP Op (Maybe Sem)
          | HoleP Ident Node
          | HoleEq Ident
          deriving Show
           
data Child = NodeC Node
           | WildC
           | NameC Ident
           | NamedNodeC Ident Node
           deriving Show
             
data Sem = NamedS Ident
         | WildS
         deriving (Show, Eq)
                  
data Op = NamedOp Ident [UIdent]
        | UnnamedOp [UIdent]
          deriving Show
             
type Ident = String
             
type UIdent = String
              
pattern :: Parser Pattern
pattern = node
          
node :: Parser Node
node = do { c1 <- enclosed child
          ; space
          ; ops <- operator
          ; space
          ; info <- optionMaybe sem
          ; c2 <- enclosed child 
          ; return $ BinP ops info c1 c2 }

       <|> try (do { ops <- operator
                   ; space
                   ; info <- optionMaybe sem
                   ; c1 <- enclosed child
                   ; space
                   ; c2 <- enclosed child
                   ; space
                   ; c3 <- enclosed child
                   ; return $ TerP ops info c1 c2 c3 })

       <|> try (do { ops <- operator
                   ; space
                   ; info <- optionMaybe sem
                   ; c <- enclosed child
                   ; return $ UnP ops info c })

       <|> try (do { ops <- operator
                   ; space
                   ; info <- optionMaybe sem
                   ; return $ NullP ops info })

       <|> try (do { string "{ }"
                   ; space
                   ; name <- ident
                   ; char '='
                   ; n <- node
                   ; return $ HoleP name n })
       <|> do { string "{ }"
              ; space
              ; string "eq"
              ; name <- enclosed ident
              ; return $ HoleEq name
              }
       
altSep :: Parser ()
altSep = space >> char '|' >> space >> return ()
         
-- [Op1 | Op2 | ...]
altOps :: Parser [UIdent]
altOps = do { char '['
            ; ops <- sepBy1 uident altSep
            ; char ']'
            ; return ops }
       
operator :: Parser Op
operator = try (do { ops <- altOps
                   ; char '@'
                   ; name <- ident
                   ; return $ NamedOp name ops })
           <|> do { ops <- altOps
                  ; return $ UnnamedOp ops }
           <|> do { op <- uident
                  ; return $ UnnamedOp [op] }
  
enclosed :: Parser a -> Parser a
enclosed p = do
  char '('
  r <- p
  char ')'
  return r

uident :: Parser UIdent
uident = do
  first <- upper
  rest <- many alphaNum
  return $ first : rest

ident :: Parser Ident
ident = do
  first <- lower
  rest <- many alphaNum
  return $ first : rest

child :: Parser Child
child = do { n <- node; return $ NodeC n }
        <|> do { wildcard; return WildC }
        <|> (try (do { name <- ident
                     ; char '='
                     ; n <- node 
                     ; return $ NamedNodeC name n }))
        <|> do { name <- ident; return $ NameC name }
        
wildcard :: Parser ()
wildcard = do
  char '_'
  return ()
           
sem :: Parser Sem
sem = do { s <- ident; space; return $ NamedS s }
      <|> do { wildcard; space; return $ WildS }
      
parsePattern :: String -> Pattern
parsePattern s = 
  case parse pattern "" s of
    Left e -> error $ "Parsing failed: " ++ (show e)
    Right p -> p
