-- | This module contains the scanner for ferry. It generates some parsers for certain tokens, and tokenizes the input
module Ferry.Front.Parser.Scanner where
    
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

lexer     = P.makeTokenParser ferryDef

-- | Ferry language definition
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
            , P.reservedOpNames= [ "\"", "%", "^", "<", "<=", ">", ">=", "==", "+", "*", "-", "/", "and", "or", "not", "contains", ".", "->"] -- operators
            , P.reservedNames  = ["ascending", "descending", "many", "one", "True", "False"
                                   , "if", "then", "else", "let", "in", "table", "with", "keys"
                                   , "relationship", "from", "to", "by", "eq", "for", "where"
                                   , "group", "order", "return", "String", "Bool", "Int", "Float"
                                   , "from", "with", "select", "into"] -- key words
            , P.caseSensitive  = True   -- ferry is case sensitive
           }

-- | initialize some utility parsers
parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
squares         = P.squares lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer
float           = P.float lexer       
natural         = P.natural lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer
comma           = P.comma lexer