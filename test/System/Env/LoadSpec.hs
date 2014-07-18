module System.Env.LoadSpec (main, spec) where

import Test.Hspec
import System.Environment (lookupEnv)

import System.Env.Load

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "loadEnv" $ do
        it "loads environment variables from ./.env if present" $ do
            writeFile ".env" $ unlines
                [ "FOO=\"bar\""
                , "BAZ=\"bat\""
                ]

            loadEnv

            mbar <- lookupEnv "FOO"
            mbat <- lookupEnv "BAZ"
            mbar `shouldBe` Just "bar"
            mbat `shouldBe` Just "bat"
