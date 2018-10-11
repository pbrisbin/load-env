module LoadEnv.Parse
    ( Environment
    , Variable
    , parseEnvironment
    , parseVariable
    ) where

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
possibly p = try (Just <$> p) <|> ignored

  where
    ignored = Nothing <$ manyTill anyToken newline

parseVariable :: Parser Variable
parseVariable = do
    optional $ between spaces spaces $ string "export"

    i <- identifier
    v <- char '=' *> value

    void $ many $ oneOf " \t"
    void newline

    pure (i, v)

-- Environment variable names used by the utilities in the Shell and Utilities
-- volume of POSIX.1-2017 consist solely of uppercase letters, digits,
-- and the <underscore> ( '_' ) from the characters defined in Portable
-- Character Set and do not begin with a digit. Other characters may be
-- permitted by an implementation; applications shall tolerate the presence
-- of such names. Uppercase and lowercase letters shall retain their unique
-- identities and shall not be folded together. The name space of environment
-- variable names containing lowercase letters is reserved for applications.
-- Applications can define any environment variables with names from this name
-- space without modifying the behavior of the standard utilities.
--
-- <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html>
--
identifier :: Parser String
identifier = do
    x <- upper <|> lower <|> underscore
    ys <- many $ upper <|>  lower <|> digit <|> underscore

    pure (x:ys)

  where
    underscore = char '_'

value :: Parser String
value = quotedValue <|> unquotedValue <|> pure ""

quotedValue :: Parser String
quotedValue = do
    q <- oneOf "'\""

    manyTill (try (escaped q) <|> anyToken) (char q)

unquotedValue :: Parser String
unquotedValue = many1 $ try (escaped ' ') <|> noneOf "\"' \n"

escaped :: Char -> Parser Char
escaped c = c <$ string ("\\" ++ [c])
