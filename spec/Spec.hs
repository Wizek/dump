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

  -- xdescribe "Debug.Dump" $ do
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
  --     [d|(1, 1), 1|] `shouldBe` "((1, 1)) = (1,1), (1) = 1"
