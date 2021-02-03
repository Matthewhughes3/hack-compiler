module Helpers where

import Data.Char

type Base = Int

startsWithDigit :: String -> Bool
startsWithDigit (s : _) = isDigit s

testList :: [[String]]
testList = [["hello", "world"], ["how", "are", "you"]]

zipListOfLists :: [[String]] -> Int -> [[(String, Int)]]
zipListOfLists [] _ = []
zipListOfLists (l : ls) x = zip l [x ..] : zipListOfLists ls (x + length l)

changeOfBase :: Base -> Int -> [Int]
changeOfBase b n = reverse $ map (`mod` b) $ takeWhile (> 0) $ map (\x -> n `div` b ^ x) [0 ..]

baseToDec :: Base -> [Int] -> Int
baseToDec _ [] = 0
baseToDec b (x : xs) = x * (b ^ length xs) + baseToDec b xs

padTo :: Int -> Char -> String -> String
padTo l p s = if length s == l then s else padTo l p (p : s)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf x l = take x l : chunksOf x (drop x l)