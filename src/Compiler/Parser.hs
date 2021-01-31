module Compiler.Parser where

import Compiler.Parser.LexicalElements
import NanoParsec

surround :: String -> String -> Parser a -> Parser a
surround s1 s2 p = do
  reservedSymbol s1
  r <- p
  reservedSymbol s2
  return r

parens :: Parser a -> Parser a
parens = surround "(" ")"

braces :: Parser a -> Parser a
braces = surround "{" "}"

squareBrackets :: Parser a -> Parser a
squareBrackets = surround "[" "]"