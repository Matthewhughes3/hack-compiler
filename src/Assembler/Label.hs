module Assembler.Label where

import NanoParsec
import Helpers
import Control.Applicative
import Data.Char

label :: Parser String
label = do
    satisfy (=='(')
    l <- some $ satisfy (\x -> x /= '(' && x /= ')')
    satisfy (==')')
    return l

aLabel :: Parser String
aLabel = do
    satisfy (=='@')
    c <- satisfy (not . isDigit)
    cs <- many $ satisfy (/='@')
    return (c:cs)