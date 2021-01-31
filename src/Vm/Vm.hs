module Vm.Vm where

import Control.Applicative
import Data.Char
import NanoParsec
import Vm.ArithmeticAndLogic
import Vm.AsmFunctions
import Vm.Branching
import Vm.Functions
import Vm.MemoryAccess

instructionParser :: String -> Int -> Parser [String]
instructionParser filename line = memoryAccessParser filename line <|> commandParser line <|> branchParser line <|> functionParser line <|> returnParser line

compile :: String -> Int -> String -> [String]
compile filename line s = case parse (instructionParser filename line) s of
  [] -> error ("Error at line " ++ show line ++ ": Invalid command " ++ s)
  [(c, _)] -> c

vm :: String -> [(String, Int)] -> String
vm filename lines = unlines $ initAddresses ++ concatMap (\(cmd, line) -> compile filename line cmd) lines