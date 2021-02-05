module Compiler.CodeGenerator.Expressions where

import Compiler.CodeGenerator.Environment
import Compiler.Parser.Expressions
import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import Control.Applicative
import Data.Char
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

-- TODO: handle Nothings
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

getVar :: Identifier -> State Environment (VarScope, Int)
getVar i = State $ \env -> case lookup (i, fn env) (st env) <|> lookup (i, cn env) (st env) of
  (Just (vs, _, vi)) -> ((vs, vi), env)

evalTerm :: Term -> State Environment Code
evalTerm (Var i) = do
  (vs, vi) <- getVar i
  return ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
evalTerm (IntegerConstant i) = return ["push constant " ++ show i]
evalTerm (StringConstant s) =
  return
    ( [ "push constant " ++ show (length s),
        "call String.new 1",
        "pop temp 1"
      ]
        ++ concatMap
          ( \c ->
              [ "push temp 1",
                "push constant " ++ show (ord c),
                "call String.appendChar 2"
              ]
          )
          s
        ++ ["push temp 1"]
    )
evalTerm (CharConstant c) = return ["push constant " ++ show (ord c)]
evalTerm (BoolConstant b) = if b then return ["push constant 1"] else return ["push constant 0"]
evalTerm This = return ["push pointer 0"]
evalTerm Null = return ["push constant 0"]
evalTerm (UnaryExpression (u, t)) = do
  tc <- evalTerm t
  return (tc ++ [unaryToOp u])
evalTerm (ArrayIndex (i, e)) = do
  (vs, vi) <- getVar i
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
  cName <- getEnv cn
  sTable <- getEnv st
  case lookup (ci, fName) sTable <|> lookup (ci, cName) sTable of
    (Just (vs, ClassType n, vi)) ->
      return
        ( ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
            ++ concat ces
            ++ ["call " ++ show n ++ "." ++ show mi ++ " " ++ show (argCount + 1)]
        )
    Nothing -> return (concat ces ++ ["call " ++ show ci ++ "." ++ show mi ++ " " ++ show argCount])

evalExpression :: Expression -> State Environment Code
evalExpression (Expression (t, ots)) = do
  let os = reverse $ map fst ots
  let ts = t : map snd ots
  cts <- concat <$> traverse evalTerm ts
  let cos = map opToArithmeticAndLogic os
  return (cts ++ cos)