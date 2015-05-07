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

identifier :: Parser String
identifier = many1 $ letter <|> char '_'

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
