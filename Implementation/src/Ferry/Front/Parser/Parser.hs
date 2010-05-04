module Ferry.Front.Parser.Parser where

import Text.ParserCombinators.Parsec
import Ferry.Front.Parser.Scanner
import Ferry.Front.Data.Language

type FParser a s = GenParser Char s (a, SourcePos)

parseFerry :: String -> String -> IO ()
parseFerry file src = do 
                        case (parse (many constParser) file src) of
                            Left err -> putStrLn (show err)
                            Right i  -> putStrLn (show i)

{-
 Parsing primitive values
-}

constParser :: FParser Const s
constParser = choice [
                      try floatParser,
                      try intParser,
                      try stringParser,
                      try boolParser                      
                      ] 

intParser :: FParser Const s
intParser = do 
             pos <- getPosition
             v <- integer
             return (CInt v, pos)
             
floatParser :: FParser Const s
floatParser = do
                pos <- getPosition
                f <- float
                return (CFloat f, pos)
                
stringParser :: FParser Const s
stringParser = do
                 pos <- getPosition
                 symbol "\""
                 s <- many (noneOf "\"")
                 symbol "\""
                 return (CString s, pos)
                 
boolParser :: FParser Const s
boolParser = try trueParser
                <|> falseParser
             
trueParser :: FParser Const s
trueParser = do
               pos <- getPosition
               reserved "True"
               return (CBool True, pos)

falseParser :: FParser Const s
falseParser = do
                pos <- getPosition
                reserved "False"
                return (CBool False, pos)