module Compiler.CodeGenerator.Statements where

import Compiler.CodeGenerator.Expressions
import Compiler.CodeGenerator.Types
import Compiler.Parser.LexicalElements
import Compiler.Parser.Statements
import Control.Applicative

makeUniqueId :: Environment -> String
makeUniqueId env = "." ++ show (cn env) ++ "." ++ show (fn env) ++ "." ++ show (lc env)

evalStatement :: Statement -> Environment -> (Code, Int)
evalStatement (LetStatement (i, ae, e)) env =
  let (Just (vs, _, vi)) = lookup (i, fn env) (st env) <|> lookup (i, cn env) (st env)
      ce = evalExpression e env
   in case ae of
        Nothing ->
          ( ce
              ++ ["pop " ++ varScopeToMemorySegment vs ++ " " ++ show vi],
            lc env
          )
        (Just ae') ->
          let cae = evalExpression ae' env
           in ( ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
                  ++ cae
                  ++ ["add"]
                  ++ ce
                  ++ [ "pop temp 0",
                       "pop pointer 1",
                       "push temp 0",
                       "pop that 0"
                     ],
                lc env
              )
evalStatement (IfElseStatement (e, ists, ests)) env =
  let ce = evalExpression e env
      (cists, lc') = foldl (\(code, lc') x -> let (c', lc'') = evalStatement x env {lc = lc'} in (code ++ c', lc'')) ([], lc env) ists
      (cests, lc'') = foldl (\(code, lc') x -> let (c', lc'') = evalStatement x env {lc = lc'} in (code ++ c', lc'')) ([], lc') ests
   in ( ce
          ++ [ "not",
               "if-goto ELSE" ++ makeUniqueId env {lc = lc''}
             ]
          ++ cists
          ++ [ "goto IF" ++ makeUniqueId env {lc = lc''},
               "label ELSE" ++ makeUniqueId env {lc = lc''}
             ]
          ++ cests
          ++ ["label IF" ++ makeUniqueId env {lc = lc''}],
        lc'' + 1
      )
evalStatement (WhileStatement (e, sts)) env =
  let ce = evalExpression e env
      (csts, lc') = foldl (\(code, lc') x -> let (c', lc'') = evalStatement x env {lc = lc'} in (code ++ c', lc'')) ([], lc env) sts
   in ( ["label WHILE" ++ makeUniqueId env {lc = lc'}]
          ++ ce
          ++ [ "not",
               "if-goto ENDWHILE" ++ makeUniqueId env {lc = lc'}
             ]
          ++ csts
          ++ [ "goto WHILE"
                 ++ makeUniqueId env {lc = lc'},
               "label ENDWHILE"
                 ++ makeUniqueId env {lc = lc'}
             ],
        lc' + 1
      )
evalStatement (DoStatement t) env = (evalTerm t env ++ ["pop temp 0"], lc env)
evalStatement (ReturnStatement e) env =
  let value = case e of
        Nothing -> ["push constant 0"]
        (Just e') -> evalExpression e' env
   in (value ++ ["return"], lc env)