module Assembler.AInstruction where

import Assembler.Bin
import Data.Char
import NanoParsec

aInstruction :: Parser String
aInstruction =
  compileA <$> do
    satisfy (== '@')
    number

compileA :: String -> String
compileA addr =
  let bin = padToSixteen . binToString $ changeOfBase 2 (read addr)
   in bin