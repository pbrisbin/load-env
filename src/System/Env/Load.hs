module System.Env.Load
    ( loadEnv
    , loadEnvFrom
    ) where

import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import System.Environment (setEnv)

import System.Env.Parse

type Environment = [Variable]

loadEnv :: IO ()
loadEnv = loadEnvFrom ".env"

loadEnvFrom :: FilePath -> IO ()
loadEnvFrom = mapM_ (uncurry setEnv) . readEnvironment <=< readFile

readEnvironment :: String -> Environment
readEnvironment = mapMaybe readVariable . lines
