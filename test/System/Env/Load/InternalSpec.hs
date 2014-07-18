module System.Env.Load.InternalSpec (main, spec) where

import Test.Hspec
import System.Env.Load.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "readVariable" $ do
        it "skips empty lines" $ do
            readVariable "" `shouldBe` Nothing

        it "skips comments" $ do
            readVariable "# comment" `shouldBe` Nothing

        it "reads unquoted variables" $ do
            readVariable "FOO=bar" `shouldBe` Just ("FOO", "bar")

        it "reads quoted variables" $ do
            readVariable "FOO=\"bar\"" `shouldBe` Just ("FOO", "bar")
            readVariable "FOO='bar'" `shouldBe` Just ("FOO", "bar")
