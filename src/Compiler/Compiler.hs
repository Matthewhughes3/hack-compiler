module Compiler.Compiler where

import Compiler.CodeGenerator.Environment
import Compiler.CodeGenerator.ProgramStructure
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import NanoParsec
import State

compile :: String -> String
compile s =
  let cl = runParser classDec s
      code = evalState (evalClass cl) Environment {st = [], fn = Identifier "", cn = Identifier "", lc = 0}
   in unlines code