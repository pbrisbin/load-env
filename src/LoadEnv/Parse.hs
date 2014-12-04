module LoadEnv.Parse
    ( Environment
    , Variable
    , parseEnvironment
    , parseVariable
    ) where

import Control.Applicative ((<$>), (<*>))
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
    v <- (,) <$> identifier <*> value
    void $ newline
    return v

identifier :: Parser String
identifier = do
    optional $ between spaces spaces $ string "export"

    i <- many1 $ letter <|> char '_'
    void $ char '='

    return i

value :: Parser String
value = do
    v <- quotedValue <|> unquotedValue <|> return ""

    void $ many $ oneOf " \t"

    return v

quotedValue :: Parser String
quotedValue = do
    q <- oneOf "'\""

    manyTill (try (escaped q) <|> anyToken) (char q)

unquotedValue :: Parser String
unquotedValue = many1 $ try (escaped ' ') <|> noneOf "\"' \n"

escaped :: Char -> Parser Char
escaped c = string ("\\" ++ [c]) >> return c
