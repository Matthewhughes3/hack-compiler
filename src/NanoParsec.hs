module NanoParsec where

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_, rs)] -> error "Parser did not consume entire stream"
        _ -> error "Parser error"

item :: Parser Char 
item = Parser $ \s ->
    case s of
        [] -> []
        (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
    fmap f (Parser cs) = Parser $ \s -> [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    return = unit
    (>>=) = bind

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

instance Alternative Parser where
    empty = mzero
    (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        [] -> parse q s
        res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
    if p c then
        unit c
    else
        failure

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser String
number = some digit

stringLiteral :: Parser String
stringLiteral = do
    satisfy (=='"')
    s <- many $ satisfy (/='"')
    satisfy (=='"')
    return s

spaces :: Parser String
spaces = many $ oneOf " \n\r"

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

oneOfS :: [String] -> Parser String
oneOfS [x] = string x
oneOfS (x:xs) = string x <|> oneOfS xs

lineComment :: Parser String
lineComment = do
    spaces
    replicateM_ 2 (satisfy (=='/'))
    many $ satisfy (/='\n')
    satisfy (=='\n')
    return []

blockComment :: Parser String
blockComment = do
    spaces
    satisfy (=='/')
    satisfy (=='*')
    many $ satisfy (/='*')
    satisfy (=='*')
    satisfy (=='/')
    return []

-- TODO
-- apiComment :: Parser String
-- apiComment = do
--     spaces 
--     satisfy (=='/')
--     replicateM_ 2 $ satisfy (=='*')
--     many $ satisfy (/='/')
--     satisfy (=='/')
--     return []

-- TODO: Fix end of file comment crashing parser
comment :: Parser String
comment = blockComment <|> lineComment

token :: Parser a -> Parser a
token p = do
    many comment
    spaces
    r <- p
    many comment
    return r