module Assembler.AInstruction where

import NanoParsec
import Data.Char
import Assembler.Bin

aInstruction :: Parser String
aInstruction = compileA <$> do
        satisfy (=='@')
        number

compileA :: String -> String
compileA addr = 
        let bin = padToSixteen . binToString $ changeOfBase 2 (read addr)
        in  bin