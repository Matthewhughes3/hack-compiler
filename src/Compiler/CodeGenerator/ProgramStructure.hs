module Compiler.CodeGenerator.ProgramStructure where

import Compiler.CodeGenerator.Environment
import Compiler.CodeGenerator.Statements
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import State

evalClass :: Class -> State Environment Code
evalClass (Class (n, cs)) = do
  setEnv setCn n
  concat <$> traverse evalClassStatement cs

getLocalVarCount :: State Environment Int
getLocalVarCount = State $ \env -> (length $ filter (\(_, (vs, _, _)) -> vs == Local) (st env), env)

evalClassStatement :: ClassStatement -> State Environment Code
evalClassStatement (ClassVarDec (vs, t, is)) = do
  let tis = map (\i -> (t, i)) is
  cName <- getEnv cn
  sTable <- getEnv st
  setEnv setSt (getVars (-1) cName (vs, tis) sTable)
  return []
evalClassStatement (FunctionDec (ft, _, fName, tis, fs)) = do
  setEnv setFn fName
  sTable <- getEnv st
  let argumentVars = case ft of
        Method -> getVars 0 fName (Argument, tis) sTable
        _ -> getVars (-1) fName (Argument, tis) sTable
  setEnv setSt (sTable ++ argumentVars)
  cfs <- concat <$> traverse evalFunctionStatement fs
  sTable' <- getEnv st
  lvc <- getLocalVarCount
  cName <- getEnv cn
  let functionHeader = "function " ++ show cName ++ "." ++ show fName ++ " " ++ show lvc
  let fullFunctionHeader =
        functionHeader : case ft of
          Constructor ->
            let fieldCount = length $ filter (\((_, n), (vs, _, _)) -> n == cName && vs == Field) sTable'
             in [ "push constant " ++ show fieldCount,
                  "call Memory.alloc 1",
                  "pop pointer 0"
                ]
          Method ->
            [ "push argument 0",
              "pop pointer 0"
            ]
          Function -> []
  return (fullFunctionHeader ++ cfs)

evalFunctionStatement :: FunctionStatement -> State Environment Code
evalFunctionStatement (FunctionVarDec (t, is)) = do
  let tis = map (\i -> (t, i)) is
  fName <- getEnv fn
  sTable <- getEnv st
  setEnv setSt (getVars (-1) fName (Local, tis) sTable)
  return []
evalFunctionStatement (FunctionStatement stm) = evalStatement stm

getVars :: Int -> Identifier -> (VarScope, [(Type, Identifier)]) -> VarTable -> VarTable
getVars _ _ (_, []) symbols = symbols
getVars index n (vs, (t, i) : tis) symbols =
  let scopeVars = filter (\((_, n'), (vs', _, _)) -> n == n' && vs == vs') symbols
      scopeIndices = map (\(_, (_, _, i)) -> i) scopeVars
      lastIndex = foldl (\acc x -> if x > acc then x else acc) index scopeIndices
   in getVars index n (vs, tis) (((i, n), (vs, t, lastIndex + 1)) : symbols)