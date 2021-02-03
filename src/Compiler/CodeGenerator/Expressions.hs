module Compiler.CodeGenerator.Expressions where

import Compiler.CodeGenerator.Types
import Compiler.Parser.Expressions
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import Control.Applicative

memorySegments :: [(VarScope, String)]
memorySegments =
  [ (Static, "static"),
    (Field, "this"),
    (Local, "local"),
    (Argument, "argument")
  ]

arithmeticAndLogic :: [(Op, String)]
arithmeticAndLogic =
  [ (Add, "add"),
    (Sub, "sub"),
    (Mul, "call Math.multiply 2"),
    (Div, "call Math.divide 2"),
    (And, "and"),
    (Or, "or"),
    (Greater, "gt"),
    (Less, "lt"),
    (Equal, "eq")
  ]

unary :: [(UnaryOp, String)]
unary =
  [ (Negative, "neg"),
    (Not, "not")
  ]

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

evalTerm :: Term -> Environment -> Code
evalTerm (Var i) env =
  let (Just (vs, _, vi)) = lookup (i, fn env) (st env) <|> lookup (i, cn env) (st env)
   in ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
evalTerm (IntegerConstant i) _ = ["push constant " ++ show i]
evalTerm (StringConstant s) _ = undefined
evalTerm (BoolConstant b) _ = if b then ["push constant 1"] else ["push constant 0"]
evalTerm This _ = ["push pointer 0"]
evalTerm Null _ = ["push constant 0"]
evalTerm (UnaryExpression (u, t)) sc =
  let tc = evalTerm t sc
      uo = unaryToOp u
   in tc ++ [uo]
evalTerm (ArrayIndex (i, e)) env =
  let (Just (vs, _, vi)) = lookup (i, fn env) (st env) <|> lookup (i, cn env) (st env)
      ce = evalExpression e env
   in ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
        ++ ce
        ++ [ "add",
             "pop pointer 1",
             "push that 0"
           ]
evalTerm (ExpressionTerm e) sc = evalExpression e sc
evalTerm (FunctionCall (Identifier n, es)) env =
  let ces = map (flip evalExpression env) es
      argCount = length ces
   in concat ces ++ ["call " ++ show (cn env) ++ "." ++ n ++ " " ++ show argCount]
evalTerm (MethodCall (ci, mi, es)) env =
  let ces = map (flip evalExpression env) es
      argCount = length ces
   in case lookup (ci, fn env) (st env) of
        (Just (vs, ClassType n, vi)) ->
          ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
            ++ concat ces
            ++ [ "call " ++ show n ++ "." ++ show mi ++ " " ++ show (argCount + 1)
               ]
        Nothing ->
          concat ces
            ++ ["call " ++ show ci ++ "." ++ show mi ++ " " ++ show argCount]

evalExpression :: Expression -> Environment -> Code
evalExpression (Expression (t, ots)) env =
  let os = map fst ots
      ts = reverse $ map snd ots
      cts = concat $ map (flip evalTerm env) ts ++ [evalTerm t env]
      cos = map opToArithmeticAndLogic os
   in cts ++ cos