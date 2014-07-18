module System.Env.Load.InternalSpec (main, spec) where

import Test.Hspec
import System.Env.Load.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "readVariable" $ do
        it "reads unquoted variables" $ do
            readVariable "FOO=bar" `shouldBe` Just ("FOO", "bar")

        it "reads quoted variables" $ do
            readVariable "FOO=\"bar\"" `shouldBe` Just ("FOO", "bar")
            readVariable "FOO='bar'" `shouldBe` Just ("FOO", "bar")

        it "handles empty values" $ do
            readVariable "FOO=" `shouldBe` Just ("FOO", "")

        it "handles empty quoted values" $ do
            readVariable "FOO=\"\"" `shouldBe` Just ("FOO", "")
            readVariable "FOO=''" `shouldBe` Just ("FOO", "")

        it "treats leading spaces as invalid" $ do
            readVariable "  FOO=bar" `shouldBe` Nothing

        it "treats spaces around equals as invalid" $ do
            readVariable "FOO = bar" `shouldBe` Nothing

        it "treats unquoted spaces as invalid" $ do
            readVariable "FOO=bar baz" `shouldBe` Nothing

        it "treats unbalanced quotes as invalid" $ do
            readVariable "FOO=\"bar" `shouldBe` Nothing
            readVariable "FOO='bar" `shouldBe` Nothing
            readVariable "FOO=bar\"" `shouldBe` Nothing
            readVariable "FOO=bar'" `shouldBe` Nothing

        it "handles escaped quotes" $ do
            readVariable "FOO=\"bar\\\"baz\"" `shouldBe` Just ("FOO", "bar\"baz")
            readVariable "FOO='bar\\'baz'" `shouldBe` Just ("FOO", "bar'baz")

        it "handles escaped spaces" $ do
            readVariable "FOO=bar\\ baz" `shouldBe` Just ("FOO", "bar baz")

        it "discards any lines using `export'" $ do
            readVariable "export FOO=bar" `shouldBe` Just ("FOO", "bar")
