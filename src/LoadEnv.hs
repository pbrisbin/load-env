-- |
--
-- This is effectively a port of dotenv, whose README explains it best:
--
-- /Storing configuration in the environment is one of the tenets of a
-- twelve-factor app. Anything that is likely to change between deployment
-- environments–such as resource handles for databases or credentials for
-- external services–should be extracted from the code into environment
-- variables./
--
-- /But it is not always practical to set environment variables on development
-- machines or continuous integration servers where multiple projects are run.
-- dotenv loads variables from a .env file into ENV when the environment is
-- bootstrapped./
--
-- <https://github.com/bkeepers/dotenv>
--
-- This library exposes functions for doing just that.
--
module LoadEnv
    ( loadEnv
    , loadEnvFrom
    ) where


import Control.Monad (when)
import LoadEnv.Parse
import System.Directory (doesFileExist)
import System.Environment (setEnv)
import Text.Parsec.String (parseFromFile)

-- | @'loadEnvFrom' \".env\"@
loadEnv :: IO ()
loadEnv = loadEnvFrom ".env"

-- | Parse the given file and set variables in the process's environment
--
-- Variables can be declared in the following form:
--
-- > FOO=bar
-- > FOO="bar"
-- > FOO='bar'
--
-- Declarations may optionally be preceded by @\"export \"@, which will be
-- ignored. Trailing whitespace is ignored. Quotes inside quoted values or
-- spaces in unquoted values must be escaped with a backlash. Invalid lines are
-- silently ignored.
--
-- __NOTE__: If the file-name is relative, the directory tree will be traversed
-- up to @\/@ looking for the file in each parent. Use @'loadEnvFromAbsolute'@
-- to avoid this.
--
loadEnvFrom :: FilePath -> IO ()
loadEnvFrom fp = do
    e <- doesFileExist fp

    when e $ parseFromFile parseEnvironment fp >>=
        either print (mapM_ $ uncurry setEnv)
