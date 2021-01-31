module Compiler.CodeGenerator.Types where

import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure

type VarTable = [((Identifier, Identifier), VarProperties)]
type VarProperties = (VarScope, Type, Int)
type Code = [String]