module Assembler.Constants where

builtinLabels :: [(String, Int)]
builtinLabels = [("R0", 0),
                 ("R1", 1),
                 ("R2", 2),
                 ("R3", 3),
                 ("R4", 4),
                 ("R5", 5),
                 ("Temp0", 5),
                 ("R6", 6),
                 ("Temp1", 6),
                 ("R7", 7),
                 ("Temp2", 7),
                 ("R8", 8),
                 ("Temp3", 8),
                 ("R9", 9),
                 ("Temp4", 9),
                 ("R10", 10),
                 ("Temp5", 10),
                 ("R11", 11),
                 ("Temp6", 11),
                 ("R12", 12),
                 ("Temp7", 12),
                 ("R13", 13),
                 ("R14", 14),
                 ("R15", 15),
                 ("SP", 0),
                 ("LCL", 1),
                 ("ARG", 2),
                 ("THIS", 3),
                 ("THAT", 4),
                 ("KEYBOARD", 32640),
                 ("SCREEN", 32704)]

labelAllocStart :: Int
labelAllocStart = 128