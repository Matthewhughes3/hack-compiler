module Compiler.Parser.Expressions where

import NanoParsec
import Compiler.Parser.LexicalElements
import Control.Applicative
import Compiler.Parser

data Term = 
    Var Identifier
  | IntegerConstant Int
  | StringConstant String
  | BoolConstant Bool
  | This
  | Null
  | UnaryExpression (UnaryOp, Term)
  | ArrayIndex (Identifier, Expression)
  | ExpressionTerm Expression
  deriving Show

data Op = 
    Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Greater
  | Less
  | Assign
  deriving Show

data UnaryOp =
    Negative
  | Tilde
  deriving Show


newtype Expression = 
    Expression (Term, [(Op, Term)])
    deriving Show

ops :: [String]
ops = ["+",
       "-",
       "*",
       "/",
       "&",
       "|",
       "<",
       ">",
       "="]

unaryOps :: [String]
unaryOps = ["-", 
            "~"]

keywordConstants :: [String]
keywordConstants = ["true",
                    "false",
                    "null",
                    "this"]

-- TODO: Factor this into IntLiteral parsing
intMax :: Int
intMax = 32767

opStringToOp :: String -> Op
opStringToOp "+" = Add
opStringToOp "-" = Sub
opStringToOp "*" = Mul
opStringToOp "/" = Div
opStringToOp "&" = And
opStringToOp "|" = Or
opStringToOp "<" = Less
opStringToOp ">" = Greater
opStringToOp "=" = Assign

unaryOpStringToUnaryOp :: String -> UnaryOp
unaryOpStringToUnaryOp "-" = Negative
unaryOpStringToUnaryOp "~" = Tilde

reservedConstant :: String -> Parser String
reservedConstant s = if s `elem` keywordConstants then token (string s) else error (s ++ " is not a valid keyword constant")

op :: Parser Op
op = token $ opStringToOp <$> oneOfS ops

unaryOp :: Parser UnaryOp
unaryOp = token $ unaryOpStringToUnaryOp <$> oneOfS unaryOps

integerConstant :: Parser Term
integerConstant = IntegerConstant . read <$> token number

stringConstant :: Parser Term
stringConstant = StringConstant <$> token stringLiteral

boolConstant :: Parser Term
boolConstant = f <$> (reservedConstant "true" <|> reservedConstant "false")
    where
        f "true" = BoolConstant True
        f "false" = BoolConstant False

thisConstant :: Parser Term
thisConstant = This <$ reservedConstant "this"

nullConstant :: Parser Term
nullConstant = Null <$ reservedConstant "null"

unaryExpression :: Parser Term
unaryExpression = do
  o <- unaryOp
  t <- term
  return (UnaryExpression (o,t))

arrayIndex :: Parser Term
arrayIndex = do
  i <- identifier
  e <- squareBrackets expression
  return (ArrayIndex (i, e))

term :: Parser Term
term = arrayIndex <|> integerConstant <|> stringConstant <|> boolConstant <|> thisConstant <|> nullConstant <|> Var <$> identifier <|> unaryExpression <|> expressionTerm

expression :: Parser Expression
expression = Expression <$> do
  a <- term
  bs <- rest []
  return (a, bs)
  where 
    rest ots = (do
        o <- op
        b <- term
        rest (ots ++ [(o,b)])) <|> return ots

expressionTerm :: Parser Term
expressionTerm = ExpressionTerm <$> parens expression
  
expressionList :: Parser [Expression]
expressionList = parens (do
    e <- expression
    es <- rest []
    return (e:es))
    where
      rest es = (do
        reservedSymbol ","
        e <- expression
        rest (e:es)) <|> return es