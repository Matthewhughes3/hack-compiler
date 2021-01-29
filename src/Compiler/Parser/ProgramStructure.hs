module Compiler.Parser.ProgramStructure where

import Compiler.Parser.LexicalElements
import Compiler.Parser.Statements
import Compiler.Parser
import Control.Applicative
import NanoParsec

newtype Class = 
    Class (Identifier, [ClassStatement])
    deriving Show

newtype ClassVar = 
    ClassVar (ClassVarType, Type, [Identifier])
    deriving Show

data ClassStatement = 
    ClassVarDec ClassVar
  | ClassFunctionDec Function
  deriving Show

newtype FunctionVar = 
    FunctionVar (Type, [Identifier])
    deriving Show

newtype Function = 
    FunctionDec (FunctionType, Maybe Type, Identifier, [(Type, Identifier)], [FunctionStatement])
    deriving Show

data FunctionStatement = 
    FunctionVarDec FunctionVar
  | FunctionStatement Statement
  deriving Show

data ClassVarType =
    Static
  | Field
  deriving Show

data FunctionType =
    Constructor
  | Function
  | Method
  deriving Show

data Type =
    IntType
  | CharType
  | BoolType
  | ClassType Identifier
  deriving Show

classVarTypes :: [(String, ClassVarType)]
classVarTypes = [("static", Static),
                 ("field", Field)]

functionTypes :: [(String, FunctionType)]
functionTypes = [("constructor", Constructor),
                 ("function", Function),
                 ("method", Method)]

types :: [(String, Type)]
types = [("int", IntType),
         ("char", CharType),
         ("bool", BoolType)]

stringToClassVarType :: String -> ClassVarType
stringToClassVarType s = case lookup s classVarTypes of
                             Nothing -> error (s ++ " is not a valid class var type")
                             (Just c) -> c

stringToFunctionType :: String -> FunctionType
stringToFunctionType s = case lookup s functionTypes of
                             Nothing -> error (s ++ " is not a valid function type")
                             (Just c) -> c

stringToType :: String -> Type
stringToType s = case lookup s types of
                             Nothing -> error (s ++ " is not a valid type")
                             (Just c) -> c

varTypes :: Parser Type
varTypes = token $ (stringToType <$> oneOfS (map fst types)) <|> (ClassType <$> identifier)

functionType :: Parser FunctionType
functionType = token $ stringToFunctionType <$> oneOfS (map fst functionTypes)

classVarType :: Parser ClassVarType
classVarType = token $ stringToClassVarType <$> oneOfS (map fst classVarTypes)

functionVar :: Parser FunctionVar
functionVar = do
    reserved "var"
    t <- varTypes
    i <- identifier
    is <- rest [i]
    reservedSymbol ";"
    return (FunctionVar (t,is))
    where
        rest is = (do
            reservedSymbol ","
            i <- identifier
            rest (is ++ [i])) <|> return is

parameterList :: Parser [(Type, Identifier)]
parameterList = (do
    t <- varTypes
    i <- identifier
    rest [(t,i)]) <|> return []
    where
        rest tis = (do
            reservedSymbol ","
            t <- varTypes
            i <- identifier
            rest (tis ++ [(t,i)])) <|> return tis

functionStatement :: Parser FunctionStatement
functionStatement = FunctionVarDec <$> functionVar <|> FunctionStatement <$> statement

function :: Parser Function
function = do
    ft <- functionType
    t <- Just <$> varTypes <|> Nothing <$ reserved "void"
    i <- identifier
    ps <- parens parameterList
    fs <- braces $ many functionStatement
    return (FunctionDec (ft, t, i, ps, fs))

classVar :: Parser ClassVar
classVar = do
    ct <- classVarType
    t <- varTypes
    i <- identifier
    is <- rest [i]
    reservedSymbol ";"
    return (ClassVar (ct,t,is))
    where 
        rest is = (do
            reservedSymbol ","
            i <- identifier
            rest (is ++ [i])) <|> return is

classStatement :: Parser ClassStatement
classStatement = ClassVarDec <$> classVar <|> ClassFunctionDec <$> function

classDec :: Parser Class
classDec = do
    reserved "class"
    i <- identifier
    cs <- braces $ many classStatement
    return (Class (i,cs))