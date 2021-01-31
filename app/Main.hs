module Main where

import Assembler.Assembler
import Vm.Vm
import Compiler.Compiler
import System.Environment
import System.IO

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
    [file] <- getArgs
    let (_, extension) = break (=='.') file
    if extension == assemblyExtension then do
        writeHex file
    else if extension == vmExtension then do
        newFile <- writeAssembly file
        writeHex newFile
    else if extension == jackExtension then do
        newFile <- writeVm file
        newFile' <- writeAssembly newFile
        writeHex newFile'
    else
        putStrLn "Must be a valid .vm or .asm file"

formatAsm :: String -> [String]
formatAsm = removeEmptyLines . map (removeWhiteSpace . removeComment) . lines

writeHex :: String -> IO ()
writeHex file = do
    contents <- formatAsm <$> readFile file
    let (filename, _) = break (=='.') file
    let newFile = filename ++ hexExtension
    writeFile newFile $ assemble contents

formatVm :: String -> [String]
formatVm  = removeEmptyLines . map removeComment . lines

writeAssembly :: String -> IO String
writeAssembly file = do
    contents <- formatVm <$> readFile file
    let (filename, _) = break (=='.') file
    let newFile = filename ++ assemblyExtension
    writeFile newFile $ vm filename (zip contents [1..])
    return newFile

writeVm :: String -> IO String
writeVm file = do
    contents <- readFile file
    let (filename, _) = break (=='.') file
    let newFile = filename ++ vmExtension
    writeFile newFile $ Compiler.Compiler.compile contents
    return newFile

removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace (x:xs) = 
    if x == ' ' then
        removeWhiteSpace xs
    else
        x:removeWhiteSpace xs

removeEmptyLines :: [String] -> [String]
removeEmptyLines [] = []
removeEmptyLines (x:xs) = 
    if null x then
        removeEmptyLines xs
    else
        x:removeEmptyLines xs

removeComment :: String -> String
removeComment [] = []
removeComment [x] = [x]
removeComment (x:y:xs) = 
    if x == '/' && y == '/' then
        removeComment []
    else
        x:removeComment (y:xs)