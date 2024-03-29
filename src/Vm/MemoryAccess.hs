module Vm.MemoryAccess where

import Control.Applicative
import Data.Char
import NanoParsec
import Vm.AsmFunctions
import Vm.Constants

memoryAccess :: [String]
memoryAccess =
  [ "push",
    "pop"
  ]

segments :: [String]
segments =
  [ "local",
    "argument",
    "this",
    "that",
    "constant",
    "static",
    "temp",
    "pointer"
  ]

memoryAccessParser :: String -> Int -> Parser [String]
memoryAccessParser filename line =
  wrapMemoryAccess filename line <$> do
    spaces
    cmd <- oneOfS memoryAccess
    spaces
    seg <- oneOfS segments
    spaces
    i <- number
    return (cmd, seg, i)

wrapMemoryAccess :: String -> Int -> (String, String, String) -> [String]
wrapMemoryAccess filename line (cmd, seg, i) =
  let fullCmd = (cmd ++ " " ++ seg ++ " " ++ i)
   in ("// " ++ fullCmd) : translateMemoryAccess filename line cmd seg i

translateMemoryAccess :: String -> Int -> String -> String -> String -> [String]
translateMemoryAccess filename line cmd seg i = case seg of
  "static" -> case cmd of
    "push" ->
      [ "@" ++ filename ++ "." ++ i,
        "D=M"
      ]
        ++ pushD
    "pop" ->
      popD
        ++ [ "@" ++ filename ++ "." ++ i,
             "M=D"
           ]
  "pointer" ->
    if i == "0" || i == "1"
      then
        let ptr = if i == "0" then this else that
         in case cmd of
              "push" ->
                [ ptr,
                  "D=M"
                ]
                  ++ pushD
              "pop" ->
                popD
                  ++ [ ptr,
                       "M=D"
                     ]
      else error ("Error at line " ++ show line ++ ": Invalid pointer index " ++ i)
  "temp" ->
    if read i >= 0 && read i <= tempMax
      then case cmd of
        "push" ->
          [ tempLabel ++ i,
            "D=M"
          ]
            ++ pushD
        "pop" ->
          popD
            ++ [ tempLabel ++ i,
                 "M=D"
               ]
      else error ("Error at line " ++ show line ++ ": Invalid temp index " ++ i)
  "constant" -> case cmd of
    "push" ->
      if read i <= constantMax
        then
          [ "@" ++ i,
            "D=A"
          ]
            ++ pushD
        else
          let i' = read i - constantMax
           in if i' <= constantMax
                then
                  [ "@" ++ show constantMax,
                    "D=A",
                    "@" ++ show i',
                    "D=D+A"
                  ]
                    ++ pushD
                else
                  let i'' = i' - constantMax
                   in [ "@" ++ show constantMax,
                        "D=A",
                        "D=D+A",
                        "@" ++ show i'',
                        "D=D+A"
                      ]
                        ++ pushD
    "pop" -> error ("Error at line " ++ show line ++ ": Cannot pop constant")
  seg' -> case lookup seg' segmentLabels of
    Nothing -> error ("Error at line " ++ show line ++ ": Invalid segment " ++ seg)
    (Just segPtr) -> case cmd of
      "push" ->
        [ "@" ++ i,
          "D=A",
          segPtr,
          "A=M+D",
          "D=M"
        ]
          ++ pushD
      "pop" ->
        [ "@" ++ i,
          "D=A",
          segPtr,
          "D=M+D",
          placeholder,
          "M=D"
        ]
          ++ popD
          ++ [ placeholder,
               "A=M",
               "M=D"
             ]