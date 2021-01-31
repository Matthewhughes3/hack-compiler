module Compiler.CodeGenerator.Expressions where

import Compiler.Parser.Expressions
import Compiler.Parser.ProgramStructure
import Compiler.Parser.LexicalElements
import Compiler.CodeGenerator.Types

memorySegments :: [(VarScope, String)]
memorySegments = 
    [(Static, "static"),
     (Field, "this"),
     (Local, "local"),
     (Argument, "argument")]

arithmeticAndLogic :: [(Op, String)]
arithmeticAndLogic = 
    [(Add, "add"),
     (Sub, "sub"),
     (Mul, "call Math.mult 2"),
     (Div, "call Math.div 2"),
     (And, "and"),
     (Or, "or"),
     (Greater, "gt"),
     (Less, "lt"),
     (Equal, "eq")]

unary :: [(UnaryOp, String)]
unary = 
    [(Negative, "neg"),
     (Not, "not")]

varScopeToMemorySegment :: VarScope -> String
varScopeToMemorySegment vs = 
    case lookup vs memorySegments of
         (Just s) -> s

opToArithmeticAndLogic :: Op -> String
opToArithmeticAndLogic o = 
    case lookup o arithmeticAndLogic of
         (Just al) -> al

unaryToOp :: UnaryOp -> String
unaryToOp u =
    case lookup u unary of
        (Just u') -> u'

evalTerm :: Identifier -> Identifier -> Term -> (VarTable, Code) -> Code
evalTerm _ fn (Var i) (symbols, _) = 
    case lookup (i,fn) symbols of
         Nothing -> error (show i ++ " has not been declared")
         (Just (vs, _, vi)) -> ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
evalTerm _ _ (IntegerConstant i) _ = ["push constant " ++ show i]
evalTerm _ _ (StringConstant s) (_, code) = undefined
evalTerm _ _ (BoolConstant b) _ = if b then ["push constant 1"] else ["push constant 0"]
evalTerm _ _ This (_, code) = undefined
evalTerm _ _ Null (_, code) = ["push constant 0"]
evalTerm cn fn (UnaryExpression (u, t)) sc = 
    let tc = evalTerm cn fn t sc
        uo = unaryToOp u
    in  tc ++ [uo]
evalTerm _ _ (ArrayIndex (i, e)) (_, code) = undefined
evalTerm cn fn (ExpressionTerm e) (symbols, code) = evalExpression cn fn e (symbols, code)
evalTerm (Identifier cn) fn (FunctionCall (Identifier n, es)) sc = 
    let ces = map (\e -> evalExpression (Identifier cn) fn e sc) es
        argCount = length ces
    in  concat ces ++ ["call " ++ cn ++ "." ++ n ++ " " ++ show argCount]
evalTerm cn fn (MethodCall (ci, Identifier mi, es)) (symbols, code) = 
    let ces = map (\e -> evalExpression cn fn e (symbols, code)) es
        (Identifier ci') = case lookup (ci, fn) symbols of 
                                Nothing -> ci
                                (Just (_, ClassType n, _)) -> n
        argCount = length ces
    in  ["push pointer 0"]
        ++ concat ces ++
        ["call " ++ ci' ++ "." ++ mi ++ " " ++ show argCount]


evalExpression :: Identifier -> Identifier -> Expression -> (VarTable, Code) -> Code
evalExpression cn fn (Expression (t, ots)) sc = 
    let os = map fst ots
        ts = reverse $ map snd ots
        cts = concat $ map (\t -> evalTerm cn fn t sc) ts ++ [evalTerm cn fn t sc]
        cos = map opToArithmeticAndLogic os
    in  cts ++ cos