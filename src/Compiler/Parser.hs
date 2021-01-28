module Compiler.Parser where

import NanoParsec
import Compiler.Parser.LexicalElements

parens :: Parser a -> Parser a
parens p = do
    reservedSymbol "("
    r <- p
    reservedSymbol ")"
    return r

braces :: Parser a -> Parser a
braces p = do
    reservedSymbol "{"
    r <- p
    reservedSymbol "}"
    return r

squareBrackets :: Parser a -> Parser a
squareBrackets p = do
    reservedSymbol "["
    r <- p
    reservedSymbol "]"
    return r