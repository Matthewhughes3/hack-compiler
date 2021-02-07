module Compiler.Parser.Statements where

import Compiler.Parser
import Compiler.Parser.Expressions
import Compiler.Parser.LexicalElements
import Control.Applicative
import NanoParsec

data Statement
  = LetStatement (Identifier, Maybe Expression, Expression)
  | IfElseStatement (Expression, [Statement], [Statement])
  | WhileStatement (Expression, [Statement])
  | DoStatement Term
  | ReturnStatement (Maybe Expression)
  deriving (Show, Eq)

letStatement :: Parser Statement
letStatement = do
  reserved "let"
  i <- identifier
  ae <- Just <$> squareBrackets expression <|> return Nothing
  reservedSymbol "="
  e <- expression
  reservedSymbol ";"
  return (LetStatement (i, ae, e))

ifElseStatement :: Parser Statement
ifElseStatement = do
  reserved "if"
  e <- parens expression
  s <- braces $ many statement
  es <- elseStatement
  return (IfElseStatement (e, s, es))
  where
    elseStatement =
      ( do
          reserved "else"
          braces $ many statement
      )
        <|> return []

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  e <- parens expression
  s <- braces $ many statement
  return (WhileStatement (e, s))

doStatement :: Parser Statement
doStatement = do
  reserved "do"
  f <- functionCall <|> methodCall
  reservedSymbol ";"
  return (DoStatement f)

returnStatement :: Parser Statement
returnStatement =
  ReturnStatement <$> do
    reserved "return"
    s <- (Just <$> expression) <|> return Nothing
    reservedSymbol ";"
    return s

statement :: Parser Statement
statement = letStatement <|> whileStatement <|> ifElseStatement <|> doStatement <|> returnStatement