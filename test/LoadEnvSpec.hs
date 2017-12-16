module LoadEnvSpec
    ( spec
    ) where

import Control.Monad (when)
import LoadEnv
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = after_ cleanup $
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

        it "does not fail if the file is not present" $ do
            loadEnvFrom "i-do-not-exist"

            return ()

cleanup :: IO ()
cleanup = do
    e <- doesFileExist envFile
    when e $ removeFile envFile

envFile :: FilePath
envFile = "/tmp/load-env-test-file"
