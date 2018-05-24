module LoadEnvSpec
    ( spec
    ) where

import Control.Monad (when)
import LoadEnv
import System.Directory
import System.Environment
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = after_ cleanup $ do
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

    describe "loadEnvFrom" $ do
        it "traverses up the directory tree" $ do
            inTempDirectory $ do
                writeFile ".env.test" "FOO=\"bar\"\n"
                inNewDirectory "foo/bar/baz" $ do
                    loadEnvFrom ".env.test"

            lookupEnv "FOO" `shouldReturn` Just "bar"

    describe "loadEnvFromAbsolute" $ do
        it "does not traverse up the directory tree" $ do
            inTempDirectory $ do
                writeFile ".env.test" "FOO=\"bar\"\n"
                inNewDirectory "foo/bar/baz" $ do
                    loadEnvFromAbsolute ".env.test"

            lookupEnv "FOO" `shouldReturn` Nothing

inTempDirectory :: IO a -> IO a
inTempDirectory f =
    withSystemTempDirectory "" $ \tmp -> withCurrentDirectory tmp f

inNewDirectory :: FilePath -> IO a -> IO a
inNewDirectory path f = do
    createDirectoryIfMissing True path
    withCurrentDirectory path f

cleanup :: IO ()
cleanup = do
    unsetEnv "FOO"
    unsetEnv "BAR"
    e <- doesFileExist envFile
    when e $ removeFile envFile

envFile :: FilePath
envFile = "/tmp/load-env-test-file"
