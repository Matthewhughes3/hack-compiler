module Vm.ArithmeticAndLogic where

import NanoParsec
import Vm.AsmFunctions
import Vm.Constants

commands :: [String]
commands = ["add",
            "sub",
            "eq",
            "gt",
            "lt",
            "and",
            "or",
            "not",
            "neg"]

commandParser :: Int -> Parser [String]
commandParser line = wrapCommand line <$> do
    spaces
    oneOfS commands

wrapCommand :: Int -> String -> [String]
wrapCommand line cmd = ("// " ++ cmd):translateCommand line cmd

translateCommand :: Int -> String -> [String]
translateCommand line cmd = case cmd of
    "add" -> popD ++
              ["A=A-1",
              "M=D+M"]
    "sub" -> popD ++
              ["A=A-1",
              "M=D-M"]
    "neg" -> [stackPointer,
              "A=M-1",
              "M=-M"]
    "eq" -> popD ++
             ["A=A-1",
             "D=D-M",
             getLabel "EQ" line,
             "D;JEQ",
             stackPointer,
             "A=M-1",
             "M=0",
             getLabel "ENDEQ" line,
             "0; JMP",
             makeLabel "EQ" line,
             stackPointer,
             "A=M-1",
             "M=1",
             makeLabel "ENDEQ" line]
    "gt" -> popD ++
             ["A=A-1",
             "D=D-M",
             getLabel "GT" line,
             "D; JGT",
             stackPointer,
             "A=M-1",
             "M=0",
             getLabel "ENDGT" line,
             "0; JMP",
             makeLabel "GT" line,
             stackPointer,
             "A=M-1",
             "M=1",
             makeLabel "ENDGT" line]
    "lt" -> popD ++
             ["A=A-1",
             "D=D-M",
             getLabel "LT" line,
             "D; JLT",
             stackPointer,
             "A=M-1",
             "M=0",
             getLabel "ENDLT" line,
             "0; JMP",
             makeLabel "LT" line,
             stackPointer,
             "A=M-1",
             "M=1",
             makeLabel "ENDLT" line]
    "and" -> popD ++
              ["A=A-1",
              "M=D&M"]
    "or" -> popD ++
             ["A=A-1",
              "M=D|M"]
    "not" -> [stackPointer,
              "A=M-1",
              "D=M",
              getLabel "FALSE" line,
              "D; JEQ",
              "D=0",
              getLabel "ENDNOT" line,
              "0; JMP",
              makeLabel "FALSE" line,
              "D=1",
              makeLabel "ENDNOT" line,
              stackPointer,
              "A=M-1",
              "M=D"]