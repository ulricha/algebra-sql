module Database.Algebra.Rewrite.PatternSyntax 
       ( Pattern
       , Op(..)
       , Child(..)
       , Sem(..)
       , Ident
       , UIdent
       , parsePattern) where

import Text.ParserCombinators.Parsec

{-

S -> Op

Op    -> (Child) Uid Sem (Child)
      |  Uid Sem (Child)
      |  Uid Sem

Child -> _
      |  Op
      |  Ident = Op 

Sem   -> _
      |  Id

-}

type Pattern = Op

data Op = BinP UIdent Sem Child Child
        | UnP UIdent Sem Child
        | NullP UIdent Sem
          deriving Show
           
data Child = OpC Op
           | WildC
           | NameC Ident
           | NamedOpC Ident Op
           deriving Show
             
data Sem = NamedS Ident
         | WildS
         deriving (Show, Eq)
             
type Ident = String

type UIdent = String
              
pattern :: Parser Pattern
pattern = operator
          
operator :: Parser Op
operator = do { c1 <- enclosed child
              ; space
              ; name <- uident
              ; space
              ; info <- sem
              ; space
              ; c2 <- enclosed child 
              ; return $ BinP name info c1 c2 }
           <|> try  (do { name <- uident
                        ; space
                        ; info <- sem
                        ; space
                        ; c <- enclosed child
                        ; return $ UnP name info c })
           <|> do { name <- uident
                  ; space
                  ; info <- sem
                  ; return $ NullP name info }
  
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
child = do { op <- operator; return $ OpC op }
        <|> do { wildcard; return WildC }
        <|> (try (do { name <- ident
                     ; char '='
                     ; op <- operator 
                     ; return $ NamedOpC name op }))
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
  
    
