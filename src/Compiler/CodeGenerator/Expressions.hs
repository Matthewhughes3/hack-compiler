module Compiler.CodeGenerator.Expressions where

import Compiler.CodeGenerator.Environment
import Compiler.Parser.Expressions
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import Control.Applicative
import State

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

getVar :: Identifier -> State Environment (Maybe (VarScope, Type, Int))
getVar i = State $ \env -> (lookup (i, fn env) (st env) <|> lookup (i, cn env) (st env), env)

evalTerm :: Term -> State Environment Code
evalTerm (Var i) = do
  -- TODO: make this beautiful
  v <- getVar i
  case v of
    Nothing -> error (show i ++ " is not defined")
    (Just (vs, _, vi)) -> return ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
evalTerm (IntegerConstant i) = return ["push constant " ++ show i]
evalTerm (StringConstant s) = undefined
evalTerm (BoolConstant b) = if b then return ["push constant 1"] else return ["push constant 0"]
evalTerm This = return ["push pointer 0"]
evalTerm Null = return ["push constant 0"]
evalTerm (UnaryExpression (u, t)) = do
  tc <- evalTerm t
  return (tc ++ [unaryToOp u])
evalTerm (ArrayIndex (i, e)) = do
  -- TODO: make this beautiful
  v <- getVar i
  case v of
    Nothing -> undefined
    (Just (vs, _, vi)) -> do
      ce <- evalExpression e
      return
        ( ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
            ++ ce
            ++ [ "add",
                 "pop pointer 1",
                 "push that 0"
               ]
        )
evalTerm (ExpressionTerm e) = evalExpression e
evalTerm (FunctionCall (Identifier n, es)) = do
  ces <- traverse evalExpression es
  let argCount = length ces
  cName <- getEnv cn
  return (concat ces ++ ["call " ++ show cName ++ "." ++ n ++ " " ++ show argCount])
evalTerm (MethodCall (ci, mi, es)) = do
  ces <- traverse evalExpression es
  let argCount = length ces
  fName <- getEnv fn
  sTable <- getEnv st
  case lookup (ci, fName) sTable of
    (Just (vs, ClassType n, vi)) ->
      return
        ( ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
            ++ concat ces
            ++ ["call " ++ show n ++ "." ++ show mi ++ " " ++ show (argCount + 1)]
        )
    Nothing -> return (concat ces ++ ["call " ++ show ci ++ "." ++ show mi ++ " " ++ show argCount])

evalExpression :: Expression -> State Environment Code
evalExpression (Expression (t, ots)) = do
  let os = map fst ots
  let ts = reverse $ map snd ots
  cts <- concat <$> traverse evalTerm ts
  ct <- evalTerm t
  let cos = map opToArithmeticAndLogic os
  return (cts ++ ct ++ cos)