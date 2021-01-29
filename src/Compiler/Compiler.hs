module Compiler.Compiler where

import Compiler.Parser.ProgramStructure
import NanoParsec

compile :: String -> String
compile s = show $ runParser classDec s