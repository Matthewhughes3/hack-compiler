module Assembler.Bin where

changeOfBase :: Int -> Int -> [Int]
changeOfBase b n = reverse $ map (`mod` b) $ takeWhile (>0) $ map (\x -> n `div` 2^x) [0..]

binToChar :: Int -> Char
binToChar 0 = '0'
binToChar 1 = '1'
binToChar x = error (show x ++ " is not valid binary")

binToString :: [Int] -> String 
binToString = map binToChar

padToSixteen :: String -> String
padToSixteen s = 
    if length s < 16 then
        padToSixteen ('0':s)
    else
        s