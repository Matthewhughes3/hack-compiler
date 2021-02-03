module Compiler.CodeGenerator.Statements where

import Compiler.CodeGenerator.Expressions
import Compiler.CodeGenerator.Types
import Compiler.Parser.LexicalElements
import Compiler.Parser.Statements
import Control.Applicative

makeUniqueId :: Environment -> String
makeUniqueId (cn, fn, _, code) = "." ++ show cn ++ "." ++ show fn ++ "." ++ show (length code)

evalStatement :: Statement -> Environment -> Code
evalStatement (LetStatement (i, ae, e)) env@(cn, fn, symbols, _) =
  let (Just (vs, _, vi)) = lookup (i, fn) symbols <|> lookup (i, cn) symbols
      ce = evalExpression e env
   in case ae of
        Nothing ->
          ce
            ++ ["pop " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
        (Just ae') ->
          let cae = evalExpression ae' env
           in ["push " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
                ++ cae
                ++ ["add"]
                ++ ce
                ++ [ "pop temp 0",
                     "pop pointer 1",
                     "push temp 0",
                     "pop that 0"
                   ]
evalStatement (IfElseStatement (e, ists, ests)) env@(cn, fn, _, code) =
  let ce = evalExpression e env
      cists = map (flip evalStatement env) ists
      cests = map (flip evalStatement env) ests
   in ce
        ++ [ "not",
             "if-goto ELSE" ++ makeUniqueId env
           ]
        ++ concat cists
        ++ [ "goto IF" ++ makeUniqueId env,
             "label ELSE" ++ makeUniqueId env
           ]
        ++ concat cests
        ++ ["label IF" ++ makeUniqueId env]
evalStatement (WhileStatement (e, sts)) env@(cn, fn, symbols, code) =
  let ce = evalExpression e env
      csts = map (flip evalStatement env) sts
   in ["label WHILE" ++ makeUniqueId env]
        ++ ce
        ++ [ "not",
             "if-goto ENDWHILE" ++ makeUniqueId env
           ]
        ++ concat csts
        ++ [ "goto WHILE"
               ++ makeUniqueId env,
             "label ENDWHILE"
               ++ makeUniqueId env
           ]
evalStatement (DoStatement t) env = evalTerm t env ++ ["pop temp 0"]
evalStatement (ReturnStatement e) env =
  let value = case e of
        Nothing -> ["push constant 0"]
        (Just e') -> evalExpression e' env
   in value ++ ["return"]