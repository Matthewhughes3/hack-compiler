module Main where

import Assembler.Assembler
import Compiler.Compiler
import Data.List
import Helpers
import System.Directory
import System.Environment
import System.IO
import Vm.Vm

jackExtension :: String
jackExtension = ".jack"

assemblyExtension :: String
assemblyExtension = ".asm"

vmExtension :: String
vmExtension = ".vm"

hexExtension :: String
hexExtension = ".hex"

main :: IO ()
main = do
  args <- getArgs
  let [file] = filter (notElem '-') args
  let filename = takeWhile (/= '.') file
  let options = sort $ concatMap (dropWhile (== '-')) $ filter (elem '-') args
  case options of
    "" -> noArgs file
    "Da" -> error "Cannot compile assembly in directory mode"
    "Dv" -> do
      files <- listDirectory file
      contents <- concat <$> mapM readFile files
      contents' <- writeAssembly contents (filename ++ "/" ++ filename)
      writeHex contents' filename
    "Dj" -> do
      files <- listDirectory file
      let readFiles = map (\f -> "./" ++ file ++ "/" ++ f) files
      let filenames = map (takeWhile (/= '.')) files
      contents <- mapM readFile readFiles
      contents' <- map lines <$> mapM (uncurry writeVm) (zip contents filenames)
      let contents'' = vmDir filenames contents'
      writeFile ("dist/" ++ filename ++ assemblyExtension) contents''
      writeHex contents'' filename
    "a" -> do
      contents <- readFile file
      writeHex contents filename
    "v" -> do
      contents <- readFile file
      contents' <- writeAssembly contents filename
      writeHex contents' filename
    "j" -> do
      contents <- readFile file
      contents' <- writeVm contents filename
      contents'' <- writeAssembly contents' filename
      writeHex contents'' filename

noArgs :: String -> IO ()
noArgs file = do
  let (filename, extension) = break (== '.') file
  if extension == assemblyExtension
    then do
      contents <- readFile file
      writeHex contents filename
    else
      if extension == vmExtension
        then do
          contents <- readFile file
          contents' <- writeAssembly contents filename
          writeHex contents' filename
        else
          if extension == jackExtension
            then do
              contents <- readFile file
              contents' <- writeVm contents filename
              contents'' <- writeAssembly contents' filename
              writeHex contents'' filename
            else putStrLn "Must be a valid .jack, .vm or .asm file"

formatAsm :: String -> [String]
formatAsm = removeEmptyLines . map (removeWhiteSpace . removeComment) . lines

writeHex :: String -> String -> IO ()
writeHex contents filename = do
  let contents' = formatAsm contents
  let newFile = filename ++ hexExtension
  writeFile ("dist/" ++ newFile) $ assemble contents'

formatVm :: String -> [String]
formatVm = removeEmptyLines . map removeComment . lines

writeAssembly :: String -> String -> IO String
writeAssembly contents filename = do
  let contents' = formatVm contents
  let newFile = filename ++ assemblyExtension
  let contents'' = vm False filename (zip contents' [1 ..])
  writeFile ("dist/" ++ newFile) contents''
  return contents''

writeVm :: String -> String -> IO String
writeVm contents filename = do
  let newFile = filename ++ vmExtension
  let contents' = Compiler.Compiler.compile contents
  writeFile ("dist/" ++ newFile) contents'
  return contents'

removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace (x : xs) =
  if x == ' '
    then removeWhiteSpace xs
    else x : removeWhiteSpace xs

removeEmptyLines :: [String] -> [String]
removeEmptyLines [] = []
removeEmptyLines (x : xs) =
  if null x
    then removeEmptyLines xs
    else x : removeEmptyLines xs

removeComment :: String -> String
removeComment [] = []
removeComment [x] = [x]
removeComment (x : y : xs) =
  if x == '/' && y == '/'
    then removeComment []
    else x : removeComment (y : xs)