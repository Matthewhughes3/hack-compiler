module Vm.Functions where

import NanoParsec
import Control.Applicative
import Data.Char
import Vm.AsmFunctions
import Vm.Constants

function :: [String]
function = ["function",
            "call"]

returnFromFunction :: String
returnFromFunction = "return"

functionParser :: Int -> Parser [String]
functionParser line = wrapFunction line <$> do
    spaces
    cmd <- oneOfS function
    spaces
    label <- some $ satisfy (/=' ')
    spaces
    argc <- number
    return (cmd, label, argc)

returnParser :: Int -> Parser [String]
returnParser line = wrapReturn line <$ do
    spaces
    string returnFromFunction

wrapFunction :: Int -> (String, String, String) -> [String]
wrapFunction line (cmd, label, argc) = ("// " ++ cmd ++ " " ++ label ++ " " ++ argc):translateFunction line cmd label argc

wrapReturn :: Int -> [String]
wrapReturn line = "// return":translateReturn line

translateFunction :: Int -> String -> String -> String -> [String]
translateFunction line cmd label argc = case cmd of
                                            "function" -> ["(" ++ label ++ ")",
                                                           "@" ++ argc,
                                                           "D=A",
                                                           makeLabel "INIT" line,
                                                           getLabel "ENDINIT" line,
                                                           "D; JEQ",
                                                           stackPointer,
                                                           "AM=M+1",
                                                           "A=A-1",
                                                           "M=0",
                                                           "D=D-1",
                                                           getLabel "INIT" line,
                                                           "0; JMP",
                                                           makeLabel "ENDINIT" line]
                                            "call" -> [getLabel returnAddress line,
                                                       "D=A"]
                                                       ++ pushD
                                                       ++ pushLabels ++
                                                       ["@5",
                                                       "D=A",
                                                       "@" ++ argc,
                                                       "D=D+A",
                                                       stackPointer,
                                                       "D=M-D",
                                                       argument,
                                                       "M=D",
                                                       stackPointer,
                                                       "D=M",
                                                       local,
                                                       "M=D",
                                                       "@" ++ label,
                                                       "0; JMP",
                                                       makeLabel returnAddress line]

translateReturn :: Int -> [String]
translateReturn line = ["@5",
                        "D=A",
                        local,
                        "A=M-D",
                        "D=M",
                        placeholder,
                        "M=D",
                        stackPointer,
                        "A=M-1",
                        "D=M",
                        argument,
                        "A=M",
                        "M=D",
                        "D=A+1",
                        stackPointer,
                        "M=D",
                        local,
                        "A=M-1",
                        "D=M",
                        that,
                        "M=D",
                        "@2",
                        "D=A",
                        local,
                        "A=M-D",
                        "D=M",
                        this,
                        "M=D",
                        "@3",
                        "D=A",
                        local,
                        "A=M-D",
                        "D=M",
                        argument,
                        "M=D",
                        "@4",
                        "D=A",
                        local,
                        "A=M-D",
                        "D=M",
                        local,
                        "M=D",
                        placeholder,
                        "A=M",
                        "0; JMP"]
