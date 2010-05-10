module Ferry.Front.Parser.Scanner where
    
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

lexer     = P.makeTokenParser ferryDef

ferryDef  = P.LanguageDef
          { 
              P.commentStart   = "{-"
            , P.commentEnd     = "-}"
            , P.commentLine    = "--"
            , P.nestedComments = True
            , P.identStart     = letter
            , P.identLetter    = alphaNum <|> oneOf "_"
            , P.opStart        = P.opLetter ferryDef
            , P.opLetter       = oneOf (concat (P.reservedOpNames ferryDef))
            , P.reservedOpNames= [ "\"", "%", "^", "<", "<=", ">", ">=", "==", "+", "*", "-", "/", "and", "or", "not", "contains", ".", "->"]
            , P.reservedNames  = ["ascending", "descending", "many", "one", "True", "False"
                                   , "if", "then", "else", "let", "in", "table", "with", "keys"
                                   , "relationship", "from", "to", "by", "eq", "for", "where"
                                   , "group", "order", "return", "String", "Bool", "Int", "Float"
                                   , "from", "with", "select", "into"]
            , P.caseSensitive  = True   
           }

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

--scanFile :: String -> String -> [Token]
--scanFile = scan