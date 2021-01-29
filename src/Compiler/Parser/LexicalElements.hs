module Compiler.Parser.LexicalElements where

import NanoParsec
import Control.Applicative
import Data.Char

newtype Identifier = 
    Identifier String
    deriving Show

symbols :: [String]
symbols = ["{",
           "}",
           "(",
           ")",
           "[",
           "]",
           ".",
           ",",
           ";",
           "~",
           "+",
           "-",
           "*",
           "/",
           "&",
           "|",
           "<",
           ">",
           "="]

keywords :: [String]
keywords = ["class",
            "constructor",
            "function",
            "method",
            "field",
            "static",
            "var",
            "int",
            "char",
            "boolean",
            "void",
            "true",
            "false",
            "null",
            "this",
            "let",
            "do",
            "if",
            "else",
            "while",
            "return"]

reserved :: String -> Parser String
reserved s = if s `elem` keywords then token (string s) else error (s ++ " is not a valid keyword")

reservedSymbol :: String -> Parser String
reservedSymbol s = if s `elem` symbols then token (string s) else error (s ++ " is not a valid symbol")

forbiddenIdentifierSymbols :: [Char]
forbiddenIdentifierSymbols = '\n':' ':map (\(c:_) -> c) symbols

identifier :: Parser Identifier
identifier = Identifier <$> token (do
    c <- satisfy (\x -> (not . isDigit) x && x `notElem` forbiddenIdentifierSymbols)
    cs <- many $ satisfy (`notElem` forbiddenIdentifierSymbols)
    let i = c:cs
    if i `elem` keywords then
        failure
    else
        return i)