module System.Env.Load
    ( loadEnv
    , loadEnvFrom
    ) where

import System.Environment (setEnv)
import Text.Parsec.String (parseFromFile)

import System.Env.Parse

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
-- Lines beginning with @#@ and blank lines are ignored. Trailing whitespace is
-- ignored. Quotes inside quoted values or spaces in unquoted values must be
-- escaped with a backlash. All else will result in a parse error being printed
-- to @stdout@.
--
-- If you wish to specify your own file, use @'loadEnvFrom'@. If you wish to
-- pass your own string or work with the parse result directly, use the
-- lower-level functions available in @"System.Env.Parse"@.
--
loadEnv :: IO ()
loadEnv = loadEnvFrom ".env"

loadEnvFrom :: FilePath -> IO ()
loadEnvFrom fp =
    parseFromFile parseEnvironment fp >>= either
        (putStrLn . show) (mapM_ $ uncurry setEnv)
