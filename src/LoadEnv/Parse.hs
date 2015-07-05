{-# LANGUAGE CPP #-}
module LoadEnv.Parse
    ( Environment
    , Variable
    , parseEnvironment
    , parseVariable
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Control.Monad (void)
import Data.Maybe (catMaybes)

import Text.Parsec
import Text.Parsec.String

type Environment = [Variable]
type Variable = (String, String)

parseEnvironment :: Parser Environment
parseEnvironment = catMaybes <$> many parseLine

parseLine :: Parser (Maybe Variable)
parseLine = possibly parseVariable

possibly :: Parser a -> Parser (Maybe a)
possibly p = try (fmap Just p) <|> ignored

  where
    ignored = do
        void $ manyTill anyToken newline
        return Nothing

parseVariable :: Parser Variable
parseVariable = do
    optional $ between spaces spaces $ string "export"

    i <- identifier
    void $ char '='

    v <- value
    void $ many $ oneOf " \t"
    void $ newline

    return (i, v)

-- Environment variable names used by the utilities in the Shell and Utilities
-- volume of IEEE Std 1003.1-2001 consist solely of uppercase letters, digits,
-- and the '_' (underscore) from the characters defined in Portable Character
-- Set and do not begin with a digit.
--
-- <http://pubs.opengroup.org/onlinepubs/000095399/basedefs/xbd_chap08.html>
--
identifier :: Parser String
identifier = do
    x <- upper <|> underscore
    ys <- many $ upper <|> digit <|> underscore

    return (x:ys)

  where
    underscore = char '_'

value :: Parser String
value = quotedValue <|> unquotedValue <|> return ""

quotedValue :: Parser String
quotedValue = do
    q <- oneOf "'\""

    manyTill (try (escaped q) <|> anyToken) (char q)

unquotedValue :: Parser String
unquotedValue = many1 $ try (escaped ' ') <|> noneOf "\"' \n"

escaped :: Char -> Parser Char
escaped c = string ("\\" ++ [c]) >> return c
