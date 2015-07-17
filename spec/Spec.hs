{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump
import Test.Hspec

main = hspec $ do
  describe "Debug.Dump" $ do
    -- it "should execute even if empty" $ do
    --   [d||] `shouldBe` ""

    it "should work with single literal" $ do
      [d|1|] `shouldBe` "(1) = 1"

    it "should work with simple expression" $ do
      [d|1 + 1|] `shouldBe` "(1 + 1) = 2"

    it "should work with bindings" $ do
      (let a = 1 in [d|a|]) `shouldBe` "(a) = 1"
      (let a = 1 in [d|a + 1|]) `shouldBe` "(a + 1) = 2"
