module System.Environment.LoadSpec (main, spec) where

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)

import Test.Hspec
import System.Environment.Load

main :: IO ()
main = hspec spec

spec :: Spec
spec = after cleanup $ do
    describe "loadEnv" $ do
        it "loads environment variables from ./.env if present" $ do
            writeFile envFile $ unlines
                [ "FOO=\"bar\""
                , "BAZ=\"bat\""
                ]

            loadEnvFrom envFile

            mbar <- lookupEnv "FOO"
            mbat <- lookupEnv "BAZ"
            mbar `shouldBe` Just "bar"
            mbat `shouldBe` Just "bat"

cleanup :: IO ()
cleanup = do
    e <- doesFileExist envFile
    when e $ removeFile envFile

envFile :: FilePath
envFile = "/tmp/load-env-test-file"
