{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Test.Hspec

import Debug.Dump
import Test.QuickCheck
import Internal.Parse
import Text.InterpolatedString.Perl6
import Utils
import Data.List (isPrefixOf)

main = spec

spec = hspec $ do


  describe "wrapInParens" $ do
    it "should work" $ do
      wrapInParens "a" `shouldBe` "(a)"
      wrapInParens "" `shouldBe` "()"

  describe "pExp" $ do
    it "handles flat" $ do
      pExp "asd" `shouldBe` "asd"
      pExp "asd,asd" `shouldBe` "asd"
      pExp "asd,a,sd" `shouldBe` "asd"

    it "handles parens" $ do
      pExp "(a,b)" `shouldBe` "(a,b)"
      pExp "(a,b) (a,b)" `shouldBe` "(a,b) (a,b)"
      pExp "(a,b), c" `shouldBe` "(a,b)"
      pExp "((a), b), c" `shouldBe` "((a), b)"
      pExp "((a,b),(a,b)), a" `shouldBe` "((a,b),(a,b))"

    it "handles char literals" $ do
      pExp "','" `shouldBe` "','"
      pExp "',', a" `shouldBe` "','"

    it "handles string literals" $ do
      pExp [q|","|] `shouldBe` [q|","|]
      pExp [q|",", a|] `shouldBe` [q|","|]

    it "handles list literals" $ do
      pExp "[a,b]" `shouldBe` "[a,b]"

    it "treats string literals as leaves" $ do
      pExp [q|"(", a|] `shouldBe` [q|"("|]

    it "support escaping in leaves" $ do
      pExp [q|"\"", a|] `shouldBe` [q|"\""|]
      pExp [q|"a" ++ "b", a|] `shouldBe` [q|"a" ++ "b"|]

    it "misc" $ do
      pExp [q|"((", a|] `shouldBe` [q|"(("|]
      pExp [q|"()(", a|] `shouldBe` [q|"()("|]
      pExp "(a, )" `shouldBe` "(a, )"
      pExp "(, b)" `shouldBe` "(, b)"

    it "shouldn't modify the list" $ do
      pExp "[" `shouldBe` "["
      pExp "([)]" `shouldBe` "([)]"
      -- pExp "([)], a" `shouldBe` "([)]"

    it "shouldn't modify the list (QuickCheck)" $ property $ let
      prop :: String -> Property
      prop s = pExp s `isPrefixOf` s
        $> counterexample [d|show s, show $ pExp s|]
      in prop

  describe "pLeaf" $ do
    it "knows to stop on regular quote" $ do
      pLeaf '"' [q|a", b|] `shouldBe` [q|a"|]

    it "knows not to terminate after escaped quote" $ do
      pLeaf '"' [q|a\"", b|] `shouldBe` [q|a\""|]

    it "does not trip up on escape characters" $ do
      pLeaf '"' [q|a\\ b|] `shouldBe` [q|a\\ b|]

  describe "separate" $ do
    it "should work" $ do
      separate "a" `shouldBe` ["a"]
      separate "a,b" `shouldBe` ["a", "b"]
      separate "a, b" `shouldBe` ["a", " b"]
      separate "(a)" `shouldBe` ["(a)"]
      separate "(a, b)" `shouldBe` ["(a, b)"]
      separate "(a, b), b" `shouldBe` ["(a, b)", " b"]
      separate "','" `shouldBe` ["','"]
      separate "a ',' b" `shouldBe` ["a ',' b"]
      separate "(a, b \")\"), b" `shouldBe` ["(a, b \")\")", " b"]

  describe "Debug.Dump" $ do
    -- TODO decide if this is useful enough to warrant support
    -- it "should execute even if empty" $ do
    --   [d||] `shouldBe` ""

    it "should work with single literal" $ do
      [d|1|] `shouldBe` "(1) = 1"

    it "should work with simple expression" $ do
      [d|1 + 1|] `shouldBe` "(1 + 1) = 2"

    it "should work with bindings" $ do
      let a = 1
      [d|a|] `shouldBe` "(a) = 1"
      [d|a + 1|] `shouldBe` "(a + 1) = 2"

    it "should work with comma separated expressions" $ do
      [d|1, 2|] `shouldBe` "(1) = 1\t  (2) = 2"
      let i = 0.25
      let f = (1 -)
      [d|i, 1/i, f i, f (1/i)|] `shouldBe`
        "(i) = 0.25\t  (1/i) = 4.0\t  (f i) = 0.75\t  (f (1/i)) = -3.0"

    it "should step over commas in sub-expressions" $ do
      [d|(1, 2), 1|] `shouldBe` "((1, 2)) = (1,2)\t  (1) = 1"
