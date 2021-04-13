module Assembler.Assembler where

import Assembler.AInstruction
import Assembler.Bin
import Assembler.CInstruction
import Assembler.Constants
import Assembler.Hex
import Assembler.Label
import Control.Applicative
import NanoParsec

assemble :: [String] -> String
assemble contents =
  let labels = storeLabels contents
      bin = (compile . replaceLabels labels labelAllocStart . removeLabels) contents
   in intelHex bin

logisimHex :: [Bin] -> String
logisimHex contents = "v2.0 raw\n" ++ foldl (\acc h -> acc ++ " " ++ show h) "" (map binToHex contents)

intelHex :: [Bin] -> String
intelHex bs = unlines $ intelH 0 bs
  where
    intelH x [] = []
    intelH x (b : bs) = intelHexLine b x : intelH (x + 2) bs

intelHexLine :: Bin -> Int -> String
intelHexLine bin startAddr =
  let addr = Hex startAddr 4
      d = showLittleEndian $ binToHex bin
      t = Hex 0 2
      byteCount = Hex 2 2
      hs = read (show (hexAppend [byteCount, addr, t]) ++ d)
      cs = checksum hs
   in ':' : show hs ++ show cs

compile :: [String] -> [Bin]
compile [] = []
compile (x : xs) =
  case parse (aInstruction <|> cInstruction) x of
    [] -> error ("Invalid command " ++ x)
    [(a, _)] -> a : compile xs

storeLabels :: [String] -> [(String, Int)]
storeLabels s = indexWalk s 0
  where
    indexWalk :: [String] -> Int -> [(String, Int)]
    indexWalk [] _ = builtinLabels
    indexWalk (x : xs) i =
      case parse label x of
        [] -> indexWalk xs (i + 1)
        [(l, _)] -> (l, i) : indexWalk xs i

replaceLabels :: [(String, Int)] -> Int -> [String] -> [String]
replaceLabels _ _ [] = []
replaceLabels labels alloc (x : xs) =
  case parse aLabel x of
    [] -> x : replaceLabels labels alloc xs
    [(label, _)] ->
      case lookup label labels of
        Nothing -> ('@' : show alloc) : replaceLabels ((label, alloc) : labels) (alloc + 1) xs
        (Just index) -> ('@' : show index) : replaceLabels labels alloc xs

removeLabels :: [String] -> [String]
removeLabels [] = []
removeLabels (x : xs) =
  case parse label x of
    [] -> x : removeLabels xs
    _ -> removeLabels xs