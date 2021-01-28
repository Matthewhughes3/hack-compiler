module Compiler.Compiler where

import Compiler.Parser.Statements
import NanoParsec

compile :: String -> String
compile s = show $ runParser statement s