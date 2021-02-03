module Compiler.CodeGenerator.ProgramStructure where

import Compiler.CodeGenerator.Statements
import Compiler.CodeGenerator.Types
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure

evalClass :: Class -> Code
evalClass (Class (n, cs)) =
  let (_, code) = foldl (\(s, c) st -> evalClassStatement n st (s, c)) ([], []) cs
   in concat code

getLocalVarCount :: VarTable -> Int
getLocalVarCount vt = length $ filter (\(_, (vs, _, _)) -> vs == Local) vt

evalClassStatement :: Identifier -> ClassStatement -> (VarTable, [Code]) -> (VarTable, [Code])
evalClassStatement cn (ClassVarDec (vs, t, is)) (symbols, code) =
  let tis = map (\i -> (t, i)) is
   in (getVar (-1) cn (vs, tis) symbols ++ symbols, code)
evalClassStatement cn (FunctionDec (ft, _, fn, tis, fs)) (symbols, code) =
  let argumentVars = case ft of
        Method -> getVar 0 fn (Argument, tis) symbols
        _ -> getVar (-1) fn (Argument, tis) symbols
      (localVars, funcCode) =
        foldl
          (\(s, c) f -> evalFunctionStatement f (Environment {cn = cn, fn = fn, st = s, code = c}))
          (argumentVars, [])
          fs
      lvc = getLocalVarCount localVars
      (Identifier cns) = cn
      (Identifier ns) = fn
      functionHeader = ("function " ++ cns ++ "." ++ ns ++ " " ++ show lvc)
      fullFunctionHeader =
        functionHeader : case ft of
          Constructor ->
            let fieldCount = length $ filter (\((_, n), (vs, _, _)) -> n == cn && vs == Field) symbols
             in [ "push constant " ++ show fieldCount,
                  "call Memory.alloc 1",
                  "pop pointer 0"
                ]
          Method ->
            [ "push argument 0",
              "pop pointer 0"
            ]
          Function -> []
      code' = fullFunctionHeader ++ funcCode
   in (localVars ++ symbols, code ++ [code'])

evalFunctionStatement :: FunctionStatement -> Environment -> (VarTable, Code)
evalFunctionStatement (FunctionVarDec (t, is)) env =
  let tis = map (\i -> (t, i)) is
   in (getVar (-1) (fn env) (Local, tis) (st env), code env)
evalFunctionStatement (FunctionStatement stm) env =
  let cst = evalStatement stm env
   in (st env, code env ++ cst)

getVar :: Int -> Identifier -> (VarScope, [(Type, Identifier)]) -> VarTable -> VarTable
getVar _ _ (_, []) symbols = symbols
getVar index n (vs, (t, i) : tis) symbols =
  let scopeVars = filter (\((_, n'), (vs', _, _)) -> n == n' && vs == vs') symbols
      scopeIndices = map (\(_, (_, _, i)) -> i) scopeVars
      lastIndex = foldl (\acc x -> if x > acc then x else acc) index scopeIndices
   in getVar index n (vs, tis) (((i, n), (vs, t, lastIndex + 1)) : symbols)