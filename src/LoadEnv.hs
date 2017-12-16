module LoadEnv
    ( loadEnv
    , loadEnvFrom
    ) where


import Control.Monad (when)
import LoadEnv.Parse
import System.Directory (doesFileExist)
import System.Environment (setEnv)
import Text.Parsec.String (parseFromFile)

-- |
--
-- Parse @./.env@ for variable declariations. Set those variables in the
-- currently running process's environment. Variables can be declared in the
-- following form:
--
-- > FOO=bar
-- > FOO="bar"
-- > FOO='bar'
--
-- Declarations may optionally be preceded by @export@, which will be ignored.
-- Trailing whitespace is ignored. Quotes inside quoted values or spaces in
-- unquoted values must be escaped with a backlash.
--
-- Invalid lines are silently ignored.
--
-- If you wish to specify your own file, use @'loadEnvFrom'@. If you wish to
-- pass your own string or work with the parse result directly, use the
-- lower-level functions available in @"LoadEnv.Parse"@.
--
loadEnv :: IO ()
loadEnv = loadEnvFrom ".env"

loadEnvFrom :: FilePath -> IO ()
loadEnvFrom fp = do
    e <- doesFileExist fp

    when e $ parseFromFile parseEnvironment fp >>=
        either print (mapM_ $ uncurry setEnv)
