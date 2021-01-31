module Compiler.Compiler where

import Compiler.Parser.ProgramStructure
import Compiler.CodeGenerator.ProgramStructure
import NanoParsec

compile :: String -> String
compile s = unlines $ evalClass $ runParser classDec s