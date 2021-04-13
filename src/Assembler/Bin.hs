module Assembler.Bin where

import Assembler.Hex
import Helpers

data Bin = Bin Int Size

instance Show Bin where
  show (Bin val size) = padTo size '0' . map decToBinDigit $ changeOfBase 2 (val `mod` 2^size)

instance Read Bin where
  readsPrec _ s =
    let v = baseToDec 2 $ map binDigitToDec s
        size = length s
     in [(Bin v size, "")]

instance Num Bin where
  b1 + b2 = binArithmetic (+) b1 b2
  b1 * b2 = binArithmetic (*) b1 b2
  b1 - b2 = binArithmetic (+) b1 (negate b2)
  negate (Bin v s) = Bin ((2 ^ s) - v) s
  abs (Bin v s) =
    if v < ((2 ^ s) `div` 2)
      then Bin v s
      else negate (Bin v s)
  signum (Bin v s) =
    if v < ((2 ^ s) `div` 2)
      then Bin 1 s
      else negate (Bin 1 s)
    
(<<<) :: Bin -> Int -> Bin
(<<<) (Bin b s) x = Bin ((b * 2^x) `mod` 2^s) s

(>>>) :: Bin -> Int -> Bin
(>>>) (Bin b s) x = Bin (b `div` 2^x) s

binToHex :: Bin -> Hex
binToHex (Bin v s) = Hex v (s `div` 4)

binFromInt :: Int -> Size -> Bin
binFromInt x size =
  if x < (2 ^ size)
    then Bin x size
    else error (show x ++ " to large to fit in binary size " ++ show size)

binArithmetic :: (Int -> Int -> Int) -> Bin -> Bin -> Bin
binArithmetic f (Bin v1 s1) (Bin v2 s2) =
  if s1 == s2
    then Bin (f v1 v2 `mod` (2 ^ s1)) s1
    else error "Cannot combine binary values of different size"

decToBinDigit :: Int -> Char
decToBinDigit 0 = '0'
decToBinDigit 1 = '1'
decToBinDigit x = error (show x ++ " is not valid binary")

binDigitToDec :: Char -> Int
binDigitToDec '0' = 0
binDigitToDec '1' = 1