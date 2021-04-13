module Vm.Branching where

import Control.Applicative
import NanoParsec
import Vm.Constants

branch :: [String]
branch =
  [ "label",
    "goto",
    "if-goto"
  ]

branchParser :: Int -> Parser [String]
branchParser line =
  wrapBranch line <$> do
    spaces
    b <- oneOfS branch
    spaces
    l <- some $ satisfy (/= ' ')
    return (b, l)

wrapBranch :: Int -> (String, String) -> [String]
wrapBranch line (cmd, label) = ("// " ++ cmd ++ " " ++ label) : translateBranch line cmd label

translateBranch :: Int -> String -> String -> [String]
translateBranch line cmd label = case cmd of
  "label" -> ["(" ++ label ++ ")"]
  "goto" ->
    [ "@" ++ label,
      "0; JMP"
    ]
  "if-goto" ->
    [ stackPointer,
      "AM=M-1",
      "D=M",
      "@" ++ label,
      "D; JGT"
    ]