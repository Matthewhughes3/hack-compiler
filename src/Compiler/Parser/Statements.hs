module Compiler.Parser.Statements where

import Compiler.Parser.LexicalElements
import Compiler.Parser.Expressions
import NanoParsec
import Compiler.Parser
import Control.Applicative

data Statement = 
    LetStatement (Identifier, Expression)
  | IfElseStatement (Expression, [Statement], [Statement])
  | WhileStatement (Expression, [Statement])
  | DoStatement Term
  | ReturnStatement (Maybe Expression)
  deriving Show

letStatement :: Parser Statement
letStatement = do
    reserved "let"
    i <- identifier
    reservedSymbol "="
    e <- expression
    reservedSymbol ";"
    return (LetStatement (i, e))

ifElseStatement :: Parser Statement
ifElseStatement = do
    reserved "if"
    e <- parens expression
    s <- braces $ many statement
    es <- elseStatement
    return (IfElseStatement (e, s, es))
    where
        elseStatement = (do
            reserved "else"
            braces $ many statement) <|> return []

whileStatement :: Parser Statement
whileStatement = do
    reserved "while"
    e <- parens expression
    s <- braces $ many statement
    return (WhileStatement (e, s))

doStatement :: Parser Statement
doStatement = do
    reserved "do"
    f <- functionCall  <|> methodCall
    reservedSymbol ";"
    return (DoStatement f)

returnStatement :: Parser Statement
returnStatement = ReturnStatement <$> do
    reserved "return"
    s <- (Just <$> expression) <|> return Nothing
    reservedSymbol ";"
    return s

statement :: Parser Statement
statement = letStatement <|> whileStatement <|> ifElseStatement <|> doStatement <|> returnStatement