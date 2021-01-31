module Compiler.Compiler where

import Compiler.CodeGenerator.ProgramStructure
import Compiler.Parser.ProgramStructure
import NanoParsec

compile :: String -> String
compile s = unlines $ evalClass $ runParser classDec s