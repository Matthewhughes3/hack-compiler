module Vm.MemoryAccess where

import NanoParsec
import Control.Applicative
import Vm.AsmFunctions
import Vm.Constants
import Data.Char

memoryAccess :: [String]
memoryAccess = ["push",
                "pop"]

segments :: [String]
segments = ["local",
            "argument",
            "this",
            "that",
            "constant",
            "static",
            "temp",
            "screen",
            "pointer"]

memoryAccessParser :: String -> Int -> Parser [String]
memoryAccessParser filename line = wrapMemoryAccess filename line <$> do
    spaces
    cmd <- oneOfS memoryAccess
    spaces
    seg <- oneOfS segments
    spaces
    i <- number
    return (cmd, seg, i)

wrapMemoryAccess :: String -> Int -> (String, String, String) -> [String]
wrapMemoryAccess filename line (cmd, seg, i) = let fullCmd = (cmd ++ " " ++ seg ++ " " ++ i)
                             in  ("// " ++ fullCmd):translateMemoryAccess filename line cmd seg i

translateMemoryAccess :: String -> Int -> String -> String -> String -> [String]
translateMemoryAccess filename line cmd seg i = case seg of
                                        "static" -> case cmd of 
                                                        "push" -> ["@" ++ filename ++ "." ++ i,
                                                                   "D=M"] ++ pushD
                                                        "pop" -> popD ++
                                                                 ["@" ++ filename ++ "." ++ i,
                                                                  "M=D"]
                                        "pointer" ->
                                            if i == "0" || i == "1" then
                                                let ptr = if i == "0" then this else that
                                                in  case cmd of
                                                        "push" -> [ptr,
                                                                   "A=M",
                                                                   "D=M"] 
                                                                   ++ pushD
                                                        "pop" -> popD ++
                                                                  [ptr,
                                                                  "A=M",
                                                                  "M=D"]
                                            else
                                                error ("Error at line " ++ show line ++ ": Invalid pointer index " ++ i)
                                        "temp" -> 
                                            if read i >= 0 && read i <= tempMax then
                                                case cmd of
                                                            "push" -> [tempLabel ++ i,
                                                                       "D=M"] 
                                                                       ++ pushD
                                                            "pop" -> popD ++ 
                                                                      [tempLabel ++ i,
                                                                      "M=D"]
                                            else
                                                error ("Error at line " ++ show line ++ ": Invalid temp index " ++ i)
                                        "constant" -> case cmd of
                                                        "push" -> ["@" ++ i,
                                                                   "D=A"]
                                                                   ++ pushD
                                                        "pop" -> error ("Error at line " ++ show line ++ ": Cannot pop constant")
                                        "screen" -> case cmd of
                                                      "push" -> ["@" ++ i,
                                                                 "D=A",
                                                                 screen,
                                                                 "A=A+D",
                                                                 "D=M"] ++ pushD
                                                      "pop" -> ["@" ++ i,
                                                                "D=A",
                                                                screen,
                                                                "D=A+D",
                                                                placeholder,
                                                                "M=D"] 
                                                                ++ popD ++
                                                                [placeholder,
                                                                "A=M",
                                                                "M=D"]
                                        seg' -> case lookup seg' segmentLabels of
                                                    Nothing -> error ("Error at line " ++ show line ++ ": Invalid segment " ++ seg)
                                                    (Just segPtr) -> case cmd of 
                                                                        "push" -> ["@" ++ i,
                                                                                   "D=A",
                                                                                   segPtr,
                                                                                   "A=M+D",
                                                                                   "D=M"]
                                                                                   ++ pushD
                                                                        "pop" -> ["@" ++ i,
                                                                                  "D=A",
                                                                                  segPtr,
                                                                                  "D=M+D",
                                                                                  placeholder,
                                                                                  "M=D"]
                                                                                  ++ popD ++
                                                                                  [placeholder,
                                                                                  "A=M",
                                                                                  "M=D"]