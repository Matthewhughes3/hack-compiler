module Assembler.CInstruction where

import Assembler.Tables
import Control.Applicative
import NanoParsec

endsWithEquals :: String -> Bool
endsWithEquals s =
  let (e : _) = drop (length s - 1) s
   in e == '='

cDest :: Parser String
cDest =
  compileCDest
    <$> ( ( do
              d <- some $ satisfy (/= '=')
              satisfy (== '=')
              return d
          )
            <|> return []
        )

cComp :: Parser String
cComp =
  compileCComp
    <$> ( do
            some $ satisfy (/= ';')
        )

cJmp :: Parser String
cJmp =
  compileCJmp
    <$> ( ( do
              satisfy (== ';')
              some $ satisfy (/= ';')
          )
            <|> return []
        )

cInstruction :: Parser String
cInstruction =
  compileCInstruction
    <$> ( do
            dest <- cDest
            comp <- cComp
            jmp <- cJmp
            return (dest, comp, jmp)
        )

compileCInstruction :: (String, String, String) -> String
compileCInstruction (dest, comp, jmp) = "111" ++ comp ++ dest ++ jmp

compileCDest :: String -> String
compileCDest s =
  if null s
    then
      let (Just n) = lookup "null" destTable
       in n
    else case lookup s destTable of
      Nothing -> error (s ++ " is not a valid destination")
      (Just bin) -> bin

compileCComp :: String -> String
compileCComp s =
  case lookup s compTable of
    Nothing -> error (s ++ " is not a valid computation")
    (Just bin) -> bin

compileCJmp :: String -> String
compileCJmp s =
  if null s
    then
      let (Just n) = lookup "null" jmpTable
       in n
    else case lookup s jmpTable of
      Nothing -> error (s ++ " is not a valid jump command")
      (Just bin) -> bin