module Assembler.Hex where

import Helpers

type Size = Int

data Hex = Hex Int Size

instance Show Hex where
  show (Hex val size) = padTo size '0' . map decToHexDigit $ changeOfBase 16 val

instance Read Hex where
  readsPrec _ s =
    let v = baseToDec 16 $ map hexDigitToDec s
        size = length s
     in [(Hex v size, "")]

instance Num Hex where
  h1 + h2 = hexArithmetic (+) h1 h2
  h1 - h2 = hexArithmetic (+) h1 (negate h2)
  h1 * h2 = hexArithmetic (*) h1 h2
  negate (Hex v s) = if v == 0 then Hex 0 s else Hex (16 ^ s - v) s
  abs (Hex v s) =
    if v < ((16 ^ s) `div` 2)
      then Hex v s
      else negate (Hex v s)
  signum (Hex v s) =
    if v < ((16 ^ s) `div` 2)
      then Hex 1 s
      else negate (Hex 1 s)

showLittleEndian :: Hex -> String
showLittleEndian (Hex v s) =
  let (f, b) = splitAt (s `div` 2) $ show (Hex v s)
   in b ++ f

hexAppend :: [Hex] -> Hex
hexAppend hs = read $ hAppend hs
  where
    hAppend [] = []
    hAppend (h : hs) = show h ++ hAppend hs

checksum :: Hex -> Hex
checksum = negate . foldl (+) (Hex 0 2) . map read . chunksOf 2 . show

hexFromInt :: Int -> Size -> Hex
hexFromInt x size =
  if x < (16 ^ size)
    then Hex x size
    else error (show x ++ " is too large to fit in hex size " ++ show size)

hexArithmetic :: (Int -> Int -> Int) -> Hex -> Hex -> Hex
hexArithmetic f (Hex v1 s1) (Hex v2 s2) =
  if s1 == s2
    then Hex (f v1 v2 `mod` (16 ^ s1)) s1
    else error "Cannot add hex values of different size"

decToHexDigit :: Int -> Char
decToHexDigit x = case lookup x decToHexDict of
  Nothing -> error (show x ++ " is too large")
  (Just h) -> h

hexDigitToDec :: Char -> Int
hexDigitToDec c =
  let (f, b) = unzip decToHexDict
      hexToDecDict = zip b f
   in case lookup c hexToDecDict of
        Nothing -> error (c : " is not valid hex")
        (Just x) -> x

decToHexDict :: [(Int, Char)]
decToHexDict =
  [ (0, '0'),
    (1, '1'),
    (2, '2'),
    (3, '3'),
    (4, '4'),
    (5, '5'),
    (6, '6'),
    (7, '7'),
    (8, '8'),
    (9, '9'),
    (10, 'A'),
    (11, 'B'),
    (12, 'C'),
    (13, 'D'),
    (14, 'E'),
    (15, 'F')
  ]