module Vm.Vm where

import Control.Applicative
import Helpers
import NanoParsec
import Vm.ArithmeticAndLogic
import Vm.AsmFunctions
import Vm.Branching
import Vm.Constants
import Vm.Functions
import Vm.MemoryAccess

instructionParser :: String -> Int -> Parser [String]
instructionParser filename line = memoryAccessParser filename line <|> commandParser line <|> branchParser line <|> functionParser line <|> returnParser line

compile :: String -> Int -> String -> [String]
compile filename line s = case parse (instructionParser filename line) s of
  [] -> error ("Error at line " ++ show line ++ ": Invalid command " ++ s)
  [(c, _)] -> c

vm :: Bool -> String -> [(String, Int)] -> String
vm singleFile filename lines = unlines $ (if singleFile then initSequence else []) ++ concatMap (\(cmd, line) -> compile filename line cmd) lines

vmDir :: [String] -> [[String]] -> String
vmDir filenames files =
  let numberedFiles = zipListOfLists files 0
   in unlines $ initSequence ++ map (uncurry (vm False)) (zip filenames numberedFiles)

initSequence :: [String]
initSequence =
  [ "// Init sequence",
    "@" ++ show stackBase,
    "D=A",
    stackPointer,
    "M=D",
    "@Sys.init",
    "0; JMP"
  ]