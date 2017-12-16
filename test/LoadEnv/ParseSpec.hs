module LoadEnv.ParseSpec
    ( spec
    ) where

import LoadEnv.Parse
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = do
    describe "parseEnvironment" $ do
        it "parses variable declarations among comments and blank lines" $ do
            let env = unlines
                    [ "# An environment file"
                    , "FOO=bar"
                    , "BAZ=\"bat\""
                    , "BAT=\"multi-"
                    , "pass"
                    , "\""
                    , ""
                    , "# vim ft:sh:"
                    ]

            parse parseEnvironment "" env `shouldBe` Right
                [ ("FOO", "bar")
                , ("BAZ", "bat")
                , ("BAT", "multi-\npass\n")
                ]

        it "parses an empty file into an empty list of variables" $ do
            parse parseEnvironment "" "" `shouldBe` Right []


    describe "parseVariable" $ do
        it "reads unquoted variables" $
            parse parseVariable "" "FOO=bar\n" `shouldBe` Right ("FOO", "bar")

        it "reads quoted variables" $ do
            parse parseVariable "" "FOO=\"bar\"\n"
                `shouldBe` Right ("FOO", "bar")
            parse parseVariable "" "FOO='bar'\n"
                `shouldBe` Right ("FOO", "bar")

        it "allows newlines in quoted variables" $ do
            parse parseVariable "" "FOO=\"foo\nbar\"\n"
                `shouldBe` Right ("FOO", "foo\nbar")

        it "handles empty values" $
            parse parseVariable "" "FOO=\n" `shouldBe` Right ("FOO", "")

        it "handles empty quoted values" $ do
            parse parseVariable "" "FOO=\"\"\n" `shouldBe` Right ("FOO", "")
            parse parseVariable "" "FOO=''\n" `shouldBe` Right ("FOO", "")

        it "handles underscored variables" $
            parse parseVariable "" "FOO_BAR=baz\n"
                `shouldBe` Right ("FOO_BAR", "baz")

        it "treats leading spaces as invalid" $
            parse parseVariable "" "  FOO=bar\n"
                `shouldContainError` "unexpected \"F\""

        it "treats spaces around equals as invalid" $
            parse parseVariable "" "FOO = bar\n"
                `shouldContainError` "unexpected \" \""

        it "treats unquoted spaces as invalid" $
            parse parseVariable "" "FOO=bar baz\n"
                `shouldContainError` "unexpected \"b\""

        it "treats unbalanced quotes as invalid" $ do
            parse parseVariable "" "FOO=\"bar\n"
                `shouldContainError` "unexpected end of input"
            parse parseVariable "" "FOO='bar\n"
                `shouldContainError` "unexpected end of input"
            parse parseVariable "" "FOO=bar\"\n"
                `shouldContainError` "unexpected \"\\\"\""
            parse parseVariable "" "FOO=bar'\n"
                `shouldContainError` "unexpected \"\'\""

        it "handles escaped quotes" $ do
            parse parseVariable "" "FOO=\"bar\\\"baz\"\n"
                `shouldBe` Right ("FOO", "bar\"baz")
            parse parseVariable "" "FOO='bar\\'baz'\n"
                `shouldBe` Right ("FOO", "bar'baz")

        it "handles escaped spaces" $
            parse parseVariable "" "FOO=bar\\ baz\n"
                `shouldBe` Right ("FOO", "bar baz")

        it "discards any lines using `export'" $
            parse parseVariable "" "export FOO=bar\n"
                `shouldBe` Right ("FOO", "bar")

        it "requires valid environment variable identifies" $ do
            parse parseVariable "" "S3_KEY=abc123\n"
                `shouldBe` Right ("S3_KEY", "abc123")
            parse parseVariable "" "_S3_KEY=abc123\n"
                `shouldBe` Right ("_S3_KEY", "abc123")

            parse parseVariable "" "S3~KEY=abc123\n"
                `shouldContainError` "unexpected \"~\""
            parse parseVariable "" "S3-KEY=abc123\n"
                `shouldContainError` "unexpected \"-\""
            parse parseVariable "" "S3_key=abc123\n"
                `shouldContainError` "unexpected \"k\""
            parse parseVariable "" "3_KEY=abc123\n"
                `shouldContainError` "unexpected \"3\""

shouldContainError :: Show a => Either a b -> String -> Expectation
v `shouldContainError` msg = either
    (\e -> show e `shouldContain` msg)
    (\_ -> expectationFailure "Expected no parse") v
