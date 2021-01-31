module Assembler.Hex where

chunksOf :: Int -> [a] -> [[a]]
chunksOf x [] = []
chunksOf x list = take x list : chunksOf x (drop x list)

binToHex :: String -> Char
binToHex bin = case lookup bin hexDict of
  Nothing -> error (bin ++ "is not valid binary")
  (Just hex) -> hex

hexToString :: [String] -> String
hexToString = foldl (\acc x -> acc ++ x ++ " ") ""

hexDict =
  [ ("0000", '0'),
    ("0001", '1'),
    ("0010", '2'),
    ("0011", '3'),
    ("0100", '4'),
    ("0101", '5'),
    ("0110", '6'),
    ("0111", '7'),
    ("1000", '8'),
    ("1001", '9'),
    ("1010", 'a'),
    ("1011", 'b'),
    ("1100", 'c'),
    ("1101", 'd'),
    ("1110", 'e'),
    ("1111", 'f')
  ]