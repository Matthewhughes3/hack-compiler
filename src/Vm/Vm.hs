module Vm.Vm where

import Control.Applicative
import NanoParsec
import Data.Char
import Vm.MemoryAccess
import Vm.ArithmeticAndLogic
import Vm.Branching
import Vm.Functions
import Vm.AsmFunctions

instructionParser :: String -> Int -> Parser [String]
instructionParser filename line = memoryAccessParser filename line <|> commandParser line <|> branchParser line <|> functionParser line <|> returnParser line

compile :: String -> Int -> String -> [String]
compile filename line s = case parse (instructionParser filename line) s of
                              [] -> error ("Error at line " ++ show line ++ ": Invalid command " ++ s)
                              [(c,_)] -> c

vm :: String -> [(String, Int)] -> String
vm filename lines = unlines $ initAddresses ++ concatMap (\(cmd, line) -> compile filename line cmd) lines