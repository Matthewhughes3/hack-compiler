module Vm.Constants where

stackPointer :: String
stackPointer = "@SP"

tempMax :: Int
tempMax = 7

tempLabel :: String
tempLabel = "@TEMP"

placeholder :: String
placeholder = "@R13"

local :: String
local = "@LCL"

argument :: String
argument = "@ARG"

this :: String
this = "@THIS"

that :: String
that = "@THAT"

screen :: String
screen = "@SCREEN"

returnAddress :: String
returnAddress = "RETURN"

segmentLabels :: [(String, String)]
segmentLabels =
  [ ("local", local),
    ("argument", argument),
    ("this", this),
    ("that", that)
  ]

baseAddresses :: [(String, String)]
baseAddresses =
  [ (stackPointer, "256"),
    (local, "1024"),
    (argument, "2048"),
    (this, "4096"),
    (that, "8192")
  ]

constantMax :: Int
constantMax = 32767