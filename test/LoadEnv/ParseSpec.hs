module LoadEnv.ParseSpec (main, spec) where

import Text.Parsec (parse)

import Test.Hspec
import Test.HUnit
import LoadEnv.Parse

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseEnvironment" $
        it "parses variable declarations among comments and blank lines" $ do
            let input = unlines
                    [ "# An environment file"
                    , "FOO=bar"
                    , "BAZ=\"bat\""
                    , ""
                    , "# vim ft:sh:"
                    ]
                expected = [("FOO", "bar"), ("BAZ", "bat")]

                result = parse parseEnvironment "" input

            case result of
                Left err -> assertFailure $ "Parse failure: " ++ show err
                Right v  -> v `shouldBe` expected

    describe "parseVariable" $ do
        it "reads unquoted variables" $
            "FOO=bar\n" `shouldParseTo` ("FOO", "bar")

        it "reads quoted variables" $ do
            "FOO=\"bar\"\n" `shouldParseTo` ("FOO", "bar")
            "FOO='bar'\n" `shouldParseTo` ("FOO", "bar")

        it "handles empty values" $
            "FOO=\n" `shouldParseTo` ("FOO", "")

        it "handles empty quoted values" $ do
            "FOO=\"\"\n" `shouldParseTo` ("FOO", "")
            "FOO=''\n" `shouldParseTo` ("FOO", "")

        it "handles underscored variables" $
            "FOO_BAR=baz\n" `shouldParseTo` ("FOO_BAR", "baz")

        it "treats leading spaces as invalid" $
            expectFailedParse "  FOO=bar\n"

        it "treats spaces around equals as invalid" $
            expectFailedParse "FOO = bar\n"

        it "treats unquoted spaces as invalid" $
            expectFailedParse "FOO=bar baz\n"

        it "treats unbalanced quotes as invalid" $ do
            expectFailedParse "FOO=\"bar\n"
            expectFailedParse "FOO='bar\n"
            expectFailedParse "FOO=bar\"\n"
            expectFailedParse "FOO=bar'\n"

        it "handles escaped quotes" $ do
            "FOO=\"bar\\\"baz\"\n" `shouldParseTo` ("FOO", "bar\"baz")
            "FOO='bar\\'baz'\n" `shouldParseTo` ("FOO", "bar'baz")

        it "handles escaped spaces" $
            "FOO=bar\\ baz\n" `shouldParseTo` ("FOO", "bar baz")

        it "discards any lines using `export'" $
            "export FOO=bar\n" `shouldParseTo` ("FOO", "bar")

shouldParseTo :: String -> Variable -> Expectation
shouldParseTo input expected =
    case parse parseVariable "" input of
        Left err -> assertFailure $ "Expected parse to succeed: " ++ show err
        Right actual -> actual `shouldBe` expected

expectFailedParse :: String -> Expectation
expectFailedParse input =
    case parse parseVariable "" input of
        Left _ -> assertBool "" True -- pass
        Right v -> assertFailure $ "Expected parse to fail: " ++ show v
