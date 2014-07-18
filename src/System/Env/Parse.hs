module System.Env.Parse
    ( Variable
    , readVariable
    , parseVariable
    ) where

import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.String

type Variable = (String, String)

readVariable :: String -> Maybe Variable
readVariable ln = case parse parseVariable "" ln of
    Left _    -> Nothing
    Right var -> Just var

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
