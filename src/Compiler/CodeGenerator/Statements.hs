module Compiler.CodeGenerator.Statements where

import Compiler.CodeGenerator.Environment
import Compiler.CodeGenerator.Expressions
import Compiler.Parser.Statements
import State

makeUniqueId :: State Environment String
makeUniqueId = State $ \env -> ("." ++ show (cn env) ++ "." ++ show (fn env) ++ "." ++ show (lc env), env)

evalStatement :: Statement -> State Environment Code
evalStatement (LetStatement i ae e) = do
  (vs, vi) <- getVar i
  ce <- evalExpression e
  case ae of
    Nothing -> return (ce ++ ["pop " ++ varScopeToMemorySegment vs ++ " " ++ show vi])
    (Just ae') -> do
      cae <- evalExpression ae'
      return
        ( ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
            ++ cae
            ++ ["add"]
            ++ ce
            ++ [ "pop temp 0",
                 "pop pointer 1",
                 "push temp 0",
                 "pop that 0"
               ]
        )
evalStatement (IfElseStatement e ists ests) = do
  lCount <- getEnv lc
  setEnv setLc (lCount + 1)
  ce <- evalExpression e
  uid <- makeUniqueId
  cists <- concat <$> traverse evalStatement ists
  cests <- concat <$> traverse evalStatement ests
  return
    ( ce
        ++ [ "not",
             "if-goto ELSE" ++ uid
           ]
        ++ cists
        ++ [ "goto ENDIF" ++ uid,
             "label ELSE" ++ uid
           ]
        ++ cests
        ++ ["label ENDIF" ++ uid]
    )
evalStatement (WhileStatement e sts) = do
  lCount <- getEnv lc
  setEnv setLc (lCount + 1)
  ce <- evalExpression e
  csts <- concat <$> traverse evalStatement sts
  uid <- makeUniqueId
  return
    ( ["label WHILE" ++ uid]
        ++ ce
        ++ [ "not",
             "if-goto ENDWHILE" ++ uid
           ]
        ++ csts
        ++ [ "goto WHILE"
               ++ uid,
             "label ENDWHILE"
               ++ uid
           ]
    )
evalStatement (DoStatement t) = do
  ct <- evalTerm t
  return (ct ++ ["pop temp 0"])
evalStatement (ReturnStatement e) =
  case e of
    Nothing -> return ["push constant 0", "return"]
    (Just e') -> do
      ce <- evalExpression e'
      return (ce ++ ["return"])