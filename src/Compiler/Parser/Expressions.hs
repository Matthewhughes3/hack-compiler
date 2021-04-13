module Compiler.Parser.Expressions where

import Compiler.Parser
import Compiler.Parser.LexicalElements
import Control.Applicative
import NanoParsec

data Term
  = Var Identifier
  | IntegerConstant Int
  | StringConstant String
  | CharConstant Char
  | BoolConstant Bool
  | This
  | Null
  | UnaryExpression UnaryOp Term
  | ArrayIndex Identifier Expression
  | ExpressionTerm Expression
  | FunctionCall Identifier [Expression]
  | MethodCall Identifier Identifier [Expression]
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Greater
  | Less
  | Equal
  deriving (Show, Eq)

data UnaryOp
  = Negative
  | Not
  deriving (Show, Eq)

data Expression
  = Expression Term [(Op, Term)]
  deriving (Show, Eq)

ops :: [(String, Op)]
ops =
  [ ("+", Add),
    ("-", Sub),
    ("*", Mul),
    ("/", Div),
    ("&", And),
    ("|", Or),
    ("<", Less),
    (">", Greater),
    ("=", Equal)
  ]

unaryOps :: [(String, UnaryOp)]
unaryOps =
  [ ("-", Negative),
    ("!", Not)
  ]

keywordConstants :: [(String, Term)]
keywordConstants =
  [ ("true", BoolConstant True),
    ("false", BoolConstant False),
    ("null", Null),
    ("this", This)
  ]

-- TODO: Factor this into IntLiteral parsing
intMax :: Int
intMax = 32767

stringToOp :: String -> Op
stringToOp s = case lookup s ops of
  Nothing -> error (s ++ " is not a valid operator")
  (Just o) -> o

stringToUnaryOp :: String -> UnaryOp
stringToUnaryOp s = case lookup s unaryOps of
  Nothing -> error (s ++ " is not a valid unary operator")
  (Just o) -> o

stringToKeywordConstant :: String -> Term
stringToKeywordConstant s = case lookup s keywordConstants of
  Nothing -> error (s ++ " is not a valid keyword constant")
  (Just o) -> o

op :: Parser Op
op = token $ stringToOp <$> oneOfS (map fst ops)

unaryOp :: Parser UnaryOp
unaryOp = token $ stringToUnaryOp <$> oneOfS (map fst unaryOps)

integerConstant :: Parser Term
integerConstant = IntegerConstant . read <$> token number

stringConstant :: Parser Term
stringConstant = StringConstant <$> token stringLiteral

charConstant :: Parser Term
charConstant = CharConstant <$> token charLiteral

keywordConstant :: Parser Term
keywordConstant = token $ stringToKeywordConstant <$> oneOfS (map fst keywordConstants)

unaryExpression :: Parser Term
unaryExpression = do
  o <- unaryOp
  t <- term
  return (UnaryExpression o t)

arrayIndex :: Parser Term
arrayIndex = do
  i <- identifier
  e <- squareBrackets expression
  return (ArrayIndex i e)

term :: Parser Term
term =
  arrayIndex
    <|> integerConstant
    <|> stringConstant
    <|> charConstant
    <|> keywordConstant
    <|> unaryExpression
    <|> expressionTerm
    <|> functionCall
    <|> methodCall
    <|> Var <$> identifier

expression :: Parser Expression
expression = do
  a <- term
  bs <- rest []
  return (Expression a bs)
  where
    rest ots =
      ( do
          o <- op
          b <- term
          rest (ots ++ [(o, b)])
      )
        <|> return ots

expressionTerm :: Parser Term
expressionTerm = ExpressionTerm <$> parens expression

expressionList :: Parser [Expression]
expressionList =
  ( do
      e <- expression
      es <- rest []
      return (e : es)
  )
    <|> return []
  where
    rest es =
      ( do
          reservedSymbol ","
          e <- expression
          rest (es ++ [e])
      )
        <|> return es

functionCall :: Parser Term
functionCall = do
  i <- identifier
  es <- parens expressionList
  return (FunctionCall i es)

methodCall :: Parser Term
methodCall = do
  c <- identifier
  reservedSymbol "."
  m <- identifier
  es <- parens expressionList
  return (MethodCall c m es)