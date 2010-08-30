-- | This module contains the scanner for ferry. It generates some parsers for certain tokens, and tokenizes the input
module Ferry.Front.Parser.Scanner where
    
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

lexer :: P.TokenParser st
lexer     = P.makeTokenParser ferryDef

-- | Ferry language definition
ferryDef :: P.LanguageDef st
ferryDef  = P.LanguageDef
          { 
              P.commentStart   = "{-"  -- We use haskell style comments
            , P.commentEnd     = "-}"
            , P.commentLine    = "--"
            , P.nestedComments = True  -- Comments can be nested
            , P.identStart     = letter -- identifiers must start with a letter (a-z)
            , P.identLetter    = alphaNum <|> oneOf "_'" -- The rest of an identifier is alphanumerical or contains _ or '
            , P.opStart        = P.opLetter ferryDef -- operators start with a letter from the reserved names
            , P.opLetter       = oneOf (concat (P.reservedOpNames ferryDef))
            , P.reservedOpNames= [ "\"", "%", "^", "<", "<=", ">", ">=", "==", "+", "*", "-", "/", "and", "or", "not", "contains", ".", "->", "()"] -- operators
            , P.reservedNames  = ["ascending", "descending", "many", "one", "True", "False"
                                   , "if", "then", "else", "let", "in", "table", "with", "keys"
                                   , "relationship", "from", "to", "by", "eq", "for", "where"
                                   , "group", "order", "return", "String", "Bool", "Int", "Float"
                                   , "from", "with", "select", "into"] -- key words
            , P.caseSensitive  = True   -- ferry is case sensitive
           }

-- | initialize some utility parsers

parens :: CharParser st a -> CharParser st a
parens          = P.parens lexer    
braces ::  CharParser st a -> CharParser st a
braces          = P.braces lexer    
semiSep ::  CharParser st a -> CharParser st [a]
semiSep         = P.semiSep lexer  
semiSep1 :: CharParser st a -> CharParser st [a]
semiSep1        = P.semiSep1 lexer    
commaSep :: CharParser st a -> CharParser st [a]
commaSep        = P.commaSep lexer
commaSep1 :: CharParser st a -> CharParser st [a]
commaSep1       = P.commaSep1 lexer
brackets :: CharParser st a -> CharParser st a
brackets        = P.brackets lexer
squares ::  CharParser st a -> CharParser st a
squares         = P.squares lexer
whiteSpace ::  CharParser st ()
whiteSpace      = P.whiteSpace lexer    
symbol :: String -> CharParser st String
symbol          = P.symbol lexer    
identifier ::  CharParser st String
identifier      = P.identifier lexer    
reserved ::  String -> CharParser st ()
reserved        = P.reserved lexer    
reservedOp :: String -> CharParser st ()
reservedOp      = P.reservedOp lexer
integer ::  CharParser st Integer
integer         = P.integer lexer
float :: CharParser st Double
float           = P.float lexer       
natural :: CharParser st Integer
natural         = P.natural lexer    
charLiteral ::  CharParser st Char
charLiteral     = P.charLiteral lexer    
stringLiteral ::  CharParser st String
stringLiteral   = P.stringLiteral lexer
comma :: CharParser st String
comma           = P.comma lexer