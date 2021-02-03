module Assembler.AInstruction where

import Assembler.Bin
import Helpers
import NanoParsec

aInstruction :: Parser Bin
aInstruction =
  compileA <$> do
    satisfy (== '@')
    number

compileA :: String -> Bin
compileA addr = Bin (read addr, 16)