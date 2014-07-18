module LoadEnv.Parse
    ( Environment
    , Variable
    , parseEnvironment
    , parseVariable
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)

import Text.Parsec
import Text.Parsec.String

type Environment = [Variable]
type Variable = (String, String)

parseEnvironment :: Parser Environment
parseEnvironment = fmap catMaybes $ many1 parseLine

parseLine :: Parser (Maybe Variable)
parseLine = try (fmap Just $ parseVariable) <|> ignoreLine

ignoreLine :: Parser (Maybe Variable)
ignoreLine = (commentLine <|> blankLine) >> return Nothing

commentLine :: Parser ()
commentLine = do
    _ <- spaces
    _ <- char '#'
    _ <- manyTill anyToken (char '\n')

    return ()

blankLine :: Parser ()
blankLine = many1 space >> return ()

parseVariable :: Parser Variable
parseVariable = (,) <$> identifier <*> value

identifier :: Parser String
identifier = do
    optional $ between spaces spaces $ string "export"

    i <- many1 letter
    _ <- char '='

    return i

value :: Parser String
value = do
    v <- quotedValue <|> unquotedValue <|> return ""
    _ <- many $ oneOf " \t"
    _ <- char '\n'

    return v

quotedValue :: Parser String
quotedValue = do
    q <- oneOf "'\""

    manyTill (try (escaped q) <|> anyToken) (char q)

unquotedValue :: Parser String
unquotedValue = many1 $ try (escaped ' ') <|> (noneOf "\"' \n")

escaped :: Char -> Parser Char
escaped c = string ("\\" ++ [c]) >> return c
