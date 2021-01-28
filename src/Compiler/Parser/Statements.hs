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

returnStatement :: Parser Statement
returnStatement = ReturnStatement <$> do
    reserved "return"
    (Just <$> expression) <|> return Nothing

statement :: Parser Statement
statement = letStatement <|> whileStatement <|> ifElseStatement <|> returnStatement