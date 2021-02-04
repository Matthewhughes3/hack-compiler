module Compiler.CodeGenerator.Environment where

import Compiler.Parser.LexicalElements
import Compiler.Parser.ProgramStructure
import State

type VarTable = [((Identifier, Identifier), VarProperties)]

type VarProperties = (VarScope, Type, Int)

type Code = [String]

data Environment = Environment
  { fn :: Identifier,
    cn :: Identifier,
    st :: VarTable,
    lc :: Int
  }

setFn :: Identifier -> Environment -> Environment
setFn i env = env {fn = i}

setCn :: Identifier -> Environment -> Environment
setCn i env = env {cn = i}

setSt :: VarTable -> Environment -> Environment
setSt s env = env {st = s}

setLc :: Int -> Environment -> Environment
setLc l env = env {lc = l}

getEnv :: (Environment -> a) -> State Environment a
getEnv f = State $ \env -> (f env, env)

setEnv :: (a -> Environment -> Environment) -> a -> State Environment ()
setEnv f a = State $ \env -> ((), f a env)