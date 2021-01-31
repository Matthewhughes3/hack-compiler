module Assembler.Label where

import Control.Applicative
import Data.Char
import Helpers
import NanoParsec

label :: Parser String
label = do
  satisfy (== '(')
  l <- some $ satisfy (\x -> x /= '(' && x /= ')')
  satisfy (== ')')
  return l

aLabel :: Parser String
aLabel = do
  satisfy (== '@')
  c <- satisfy (not . isDigit)
  cs <- many $ satisfy (/= '@')
  return (c : cs)