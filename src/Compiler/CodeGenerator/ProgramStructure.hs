module Compiler.CodeGenerator.ProgramStructure where

import Compiler.Parser.ProgramStructure
import Compiler.Parser.LexicalElements
import Compiler.CodeGenerator.Types
import Compiler.CodeGenerator.Statements

evalClass :: Class ->  Code
evalClass (Class (n, cs)) = 
    let (_, code) = foldl (\(s, c) st -> evalClassStatement n st (s, c)) ([],[]) cs
    in  ["call Main.main 0",
            "goto END"]
        ++ concat code ++
        ["label END",
            "goto END"]

getLocalVarCount :: VarTable -> Int
getLocalVarCount vt = length $ filter (\(_, (vs, _, _)) -> vs == Local) vt

evalClassStatement :: Identifier -> ClassStatement -> (VarTable, [Code]) -> (VarTable, [Code])
evalClassStatement n (ClassVarDec (vs, t, is)) (symbols, code) = 
    let tis = map (\i -> (t, i)) is
    in  (getVar n (vs, tis) [] ++ symbols, code)
evalClassStatement cn (FunctionDec (_, _, fn, tis, fs)) (symbols, code) = 
    let argumentVars = getVar fn (Argument, tis) []
        (localVars, funcCode) = foldl 
            (\(s, c) f -> evalFunctionStatement cn fn f (s, c)) 
            (argumentVars, []) fs
        lvc = getLocalVarCount localVars
        (Identifier cns) = cn
        (Identifier ns) = fn
        code' = ("function " ++ cns ++ "." ++ ns ++ " " ++ show lvc):funcCode
    in  (localVars ++ symbols, code ++ [code'])

evalFunctionStatement :: Identifier -> Identifier -> FunctionStatement -> (VarTable, Code) -> (VarTable, Code)
evalFunctionStatement cn n (FunctionVarDec (t, is)) (symbols, code) = 
    let tis = map (\i -> (t, i)) is
    in  (getVar n (Local, tis) symbols, code)
evalFunctionStatement cn fn (FunctionStatement st) (symbols, code) = 
    let cst = evalStatement cn fn st (symbols, code)
    in  (symbols, code ++ cst)

getVar :: Identifier -> (VarScope, [(Type, Identifier)]) -> VarTable -> VarTable
getVar _ (_, []) symbols = symbols
getVar n (vs, (t,i):tis) symbols = 
    let scopeVars = filter (\((_,n'), (vs',_,_)) -> n == n' && vs == vs') symbols
        scopeIndices = map (\(_, (_,_,i)) -> i) scopeVars
        lastIndex = foldl (\acc x -> if x > acc then x else acc) (-1) scopeIndices
    in  getVar n (vs, tis) (((i, n), (vs, t, lastIndex + 1)):symbols)