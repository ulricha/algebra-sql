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

data Node = BinP Op Sem Child Child
          | UnP Op Sem Child
          | NullP Op Sem
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
          ; info <- sem
          ; space
          ; c2 <- enclosed child 
          ; return $ BinP ops info c1 c2 }
       <|> try  (do { ops <- operator
                    ; space
                    ; info <- sem
                    ; space
                    ; c <- enclosed child
                    ; return $ UnP ops info c })
       <|> do { ops <- operator
              ; space
              ; info <- sem
              ; return $ NullP ops info }
       
altSep :: Parser ()
altSep = space >> char '|' >> space >> return ()
         
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
sem = (ident >>= (return . NamedS))
      <|> (wildcard >> (return WildS))
      
parsePattern :: String -> Pattern
parsePattern s = 
  case parse pattern "" s of
    Left e -> error $ "Parsing failed: " ++ (show e)
    Right p -> p
