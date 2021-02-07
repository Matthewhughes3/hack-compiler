module Vm.AsmFunctions where

import Vm.Constants

pushD :: [String]
pushD =
  [ stackPointer,
    "AM=M+1",
    "A=A-1",
    "M=D"
  ]

popD :: [String]
popD =
  [ stackPointer,
    "AM=M-1",
    "D=M"
  ]

pushLabel :: String -> [String]
pushLabel label =
  [ label,
    "D=M"
  ]
    ++ pushD

pushLabels :: [String]
pushLabels = concatMap (\(_, label) -> pushLabel label) segmentLabels

makeLabel :: String -> Int -> String
makeLabel label line = "(" ++ label ++ show line ++ ")"

getLabel :: String -> Int -> String
getLabel label line = "@" ++ label ++ show line