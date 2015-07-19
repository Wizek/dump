{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Debug.Dump
import Utils

-- xdescribe n = describe n $ it "" pending
-- main = pTest
-- main = print 123

main = spec

spec = hspec $ do
  describe "wrapInParens" $ do
    it "should work" $ do
      wrapInParens "a" `shouldBe` "(a)"
      wrapInParens "" `shouldBe` "()"

  describe "separate" $ do
    it "should work" $ do
      -- separate "a" `shouldBe` ["a"]
      -- separate "a,b" `shouldBe` ["a", "b"]
      -- separate "a, b" `shouldBe` ["a", "b"]
      separate "(a)" `shouldBe` ["(a)"]
      -- separate "(a, b)" `shouldBe` ["(a, b)"]

  -- describe "Debug.Dump" $ do
  --   -- TODO decide if this is useful enough to warrant support
  --   -- it "should execute even if empty" $ do
  --   --   [d||] `shouldBe` ""

  --   it "should work with single literal" $ do
  --     [d|1|] `shouldBe` "(1) = 1"

  --   it "should work with simple expression" $ do
  --     [d|1 + 1|] `shouldBe` "(1 + 1) = 2"

  --   it "should work with bindings" $ do
  --     let a = 1
  --     [d|a|] `shouldBe` "(a) = 1"
  --     [d|a + 1|] `shouldBe` "(a + 1) = 2"

  --   it "should work with comma separated expressions" $ do
  --     [d|1, 2|] `shouldBe` "(1) = 1, (2) = 2"
  --     let i = 0.25
  --     let f = (1 -)
  --     [d|i, 1/i, f i, f (1/i)|] `shouldBe`
  --       "(i) = 0.25, (1/i) = 4.0, (f i) = 0.75, (f (1/i)) = -3.0"

  --   -- TODO parser, WIP on 'parse' branch
  --   -- it "should step over commas in sub-expressions" $ do
  --   --   [d|(1, 2), 1|] `shouldBe` "((1, 1)) = (1,1), (1) = 1"
