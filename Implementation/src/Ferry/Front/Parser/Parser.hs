module Ferry.Front.Parser.Parser where

import Text.ParserCombinators.UU.Parsing(Applicative((<*>)), Error,
                                         Str, P, Symbol(..), (<<|>), ExtApplicative((<*), (*>)),
                                         pEnd, parse, listToStr, pList, pList1, (<$>),
                                         Alternative((<|>)))
import UU.Scanner.TokenParser
import UU.Scanner.Token (Token)

type Parser s a = P (Str s) a  -- type synonym for parser type

execParser p tokens = parse ((,) <$> pEnd ) (listToStr tokens)

intParser :: Parser Token String
intParser = pInteger16