module Compiler.Parser.ProgramStructure where

import Compiler.Parser
import Compiler.Parser.LexicalElements
import Compiler.Parser.Statements
import Control.Applicative
import NanoParsec

newtype Class
  = Class (Identifier, [ClassStatement])
  deriving (Show, Eq)

data ClassStatement
  = ClassVarDec (VarScope, Type, [Identifier])
  | FunctionDec (FunctionType, Maybe Type, Identifier, [(Type, Identifier)], [FunctionStatement])
  deriving (Show, Eq)

data VarScope
  = Static
  | Field
  | Argument
  | Local
  deriving (Show, Eq)

data FunctionStatement
  = FunctionVarDec (Type, [Identifier])
  | FunctionStatement Statement
  deriving (Show, Eq)

data FunctionType
  = Constructor
  | Function
  | Method
  deriving (Show, Eq)

data Type
  = IntType
  | CharType
  | BoolType
  | ClassType Identifier
  deriving (Show, Eq)

varScopeTypes :: [(String, VarScope)]
varScopeTypes =
  [ ("static", Static),
    ("field", Field),
    ("local", Local),
    ("argument", Argument)
  ]

functionTypes :: [(String, FunctionType)]
functionTypes =
  [ ("constructor", Constructor),
    ("function", Function),
    ("method", Method)
  ]

types :: [(String, Type)]
types =
  [ ("int", IntType),
    ("char", CharType),
    ("bool", BoolType)
  ]

stringToVarScope :: String -> VarScope
stringToVarScope s = case lookup s varScopeTypes of
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

varScope :: Parser VarScope
varScope = token $ stringToVarScope <$> oneOfS (map fst varScopeTypes)

functionVar :: Parser FunctionStatement
functionVar = do
  reserved "var"
  t <- varTypes
  i <- identifier
  is <- rest [i]
  reservedSymbol ";"
  return (FunctionVarDec (t, is))
  where
    rest is =
      ( do
          reservedSymbol ","
          i <- identifier
          rest (is ++ [i])
      )
        <|> return is

parameterList :: Parser [(Type, Identifier)]
parameterList =
  ( do
      t <- varTypes
      i <- identifier
      rest [(t, i)]
  )
    <|> return []
  where
    rest tis =
      ( do
          reservedSymbol ","
          t <- varTypes
          i <- identifier
          rest (tis ++ [(t, i)])
      )
        <|> return tis

functionStatement :: Parser FunctionStatement
functionStatement = functionVar <|> FunctionStatement <$> statement

function :: Parser ClassStatement
function = do
  ft <- functionType
  t <- Just <$> varTypes <|> Nothing <$ reserved "void"
  i <- identifier
  ps <- parens parameterList
  fs <- braces $ many functionStatement
  return (FunctionDec (ft, t, i, ps, addImplicitReturn fs))

-- TODO: Saturate this function with relevant info, like line no. and adapt it to throw an error if the function isn't void
addImplicitReturn :: [FunctionStatement] -> [FunctionStatement]
addImplicitReturn fs = let hasReturn = foldl (\hr f -> isReturn f || hr) False fs
                       in  if hasReturn then fs else fs ++ [FunctionStatement (ReturnStatement Nothing)]

isReturn :: FunctionStatement -> Bool
isReturn f = 
  case f of
    FunctionStatement (ReturnStatement _) -> True
    _ -> False

classVar :: Parser ClassStatement
classVar = do
  vs <- varScope
  if vs /= Static && vs /= Field
    then error ("Invalid class var scope " ++ show vs)
    else do
      t <- varTypes
      i <- identifier
      is <- rest [i]
      reservedSymbol ";"
      return (ClassVarDec (vs, t, is))
  where
    rest is =
      ( do
          reservedSymbol ","
          i <- identifier
          rest (is ++ [i])
      )
        <|> return is

classStatement :: Parser ClassStatement
classStatement = classVar <|> function

classDec :: Parser Class
classDec = do
  reserved "class"
  i <- identifier
  cs <- braces $ many classStatement
  return (Class (i, cs))