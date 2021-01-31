module Compiler.CodeGenerator.Statements where

import Compiler.CodeGenerator.Expressions
import Compiler.CodeGenerator.Types
import Compiler.Parser.LexicalElements
import Compiler.Parser.Statements

evalStatement :: Identifier -> Identifier -> Statement -> (VarTable, Code) -> Code
evalStatement cn fn (LetStatement (i, e)) (symbols, code) =
  let (vs, _, vi) = case lookup (i, fn) symbols of
        Nothing -> case lookup (i, cn) symbols of
          Nothing -> error (show i ++ " has not been declared")
          (Just x) -> x
        (Just x) -> x
   in evalExpression cn fn e (symbols, code)
        ++ ["pop " ++ varScopeToMemorySegment vs ++ " " ++ show vi]
evalStatement cn fn (IfElseStatement (e, ists, ests)) (symbols, code) =
  let ce = evalExpression cn fn e (symbols, code)
      cists = map (\st -> evalStatement cn fn st (symbols, code)) ists
      cests = map (\st -> evalStatement cn fn st (symbols, code)) ests
   in ce
        ++ [ "not",
             "if-goto L1." ++ show (length code)
           ]
        ++ concat cists
        ++ [ "goto L2." ++ show (length code),
             "label L1." ++ show (length code)
           ]
        ++ concat cests
        ++ ["label L2." ++ show (length code)]
evalStatement cn fn (WhileStatement (e, sts)) (symbols, code) =
  let ce = evalExpression cn fn e (symbols, code)
      csts = map (\st -> evalStatement cn fn st (symbols, code)) sts
   in ["label L1." ++ show (length code)]
        ++ ce
        ++ [ "not",
             "if-goto L2." ++ show (length code)
           ]
        ++ concat csts
        ++ [ "goto L1." ++ show (length code),
             "label L2." ++ show (length code)
           ]
evalStatement cn fn (DoStatement t) sc = evalTerm cn fn t sc ++ ["pop temp 0"]
evalStatement cn fn (ReturnStatement e) sc =
  let value = case e of
        Nothing -> ["push constant 0"]
        (Just e') -> evalExpression cn fn e' sc
   in value ++ ["return"]