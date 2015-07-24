{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Debug.Dump
import Test.QuickCheck
import Text.InterpolatedString.Perl6
import Data.List (isPrefixOf)

import Internal.Parser
import Internal.Utils

main = spec

spec = hspec $ do


  describe "wrapInParens" $ do
    it "should work" $ do
      wrapInParens "a" `shouldBe` "(a)"
      wrapInParens "" `shouldBe` "()"

  describe "fff" $ do
    it "should work" $ do
      fff "asd" [] `shouldBe` ("asd", "")
      fff "a,b" [] `shouldBe` ("a", "b")
      fff "a,b" ")" `shouldBe` ("a,b", "")
      fff "(a,b)" [] `shouldBe` ("(a,b)", "")
      fff "(a,b),c" [] `shouldBe` ("(a,b)", "c")
      fff "[a,b],c" [] `shouldBe` ("[a,b]", "c")
      fff "],b],c" "]]" `shouldBe` ("],b]", "c")
      fff "],b],c" "]]" `shouldBe` ("],b]", "c")
      -- fff [q|a,b",c|] ['"'] `shouldBe` ([q|a,b"|], "c")
      fff [q|"a,b",c|] [] `shouldBe` ([q|"a,b"|], "c")
      -- fff [q|(a",c|] ['"'] `shouldBe` ([q|(a"|], "c")
      -- fff [q|,',c|] ['\''] `shouldBe` ([q|,'|], "c")
      fff [q|',',c|] [] `shouldBe` ([q|','|], "c")
      fff [q|'"',c|] [] `shouldBe` ([q|'"'|], "c")
      -- fff [q|'\',',|] [] `shouldBe` ([q|'\','|], "c")
      fff [q|"\",",c|] [] `shouldBe` ([q|"\","|], "c")
      fff [q|'\'',c|] [] `shouldBe` ([q|'\''|], "c")
      fff [q|(\),c|] [] `shouldBe` ([q|(\)|], "c")

  -- describe "parseLeafUntil" $ do
  --   let p = parseLeafUntil '"'
  --   it "knows to stop on regular quote" $ do
  --     p [q|a"   |] `shouldBe` ([q|a"|], "   ")

  --   it "knows not to terminate after escaped quote" $ do
  --     p [q|a\""   |] `shouldBe` ([q|a\""|], "   ")

  --   it "does not trip up on escape characters" $ do
  --     p [q|a\\  |] `shouldBe` ([q|a\\  |], "")

  --   it "does not trip up on escape character at the end" $ do
  --     p [q|a\\|] `shouldBe` ([q|a\\|], "")

  -- describe "parseExpr" $ do
  --   let p = parseExpr .> fst
  --   it "handles flat" $ do
  --     p "asd" `shouldBe` "asd"
  --     p "asd,asd" `shouldBe` "asd"
  --     p "asd,a,sd" `shouldBe` "asd"

  --   it "handles parens" $ do
  --     p "(a,b)" `shouldBe` "(a,b)"
  --     p "(a,b) (a,b)" `shouldBe` "(a,b) (a,b)"
  --     p "(a,b), c" `shouldBe` "(a,b)"
  --     p "((a), b), c" `shouldBe` "((a), b)"
  --     p "((a,b),(a,b)), a" `shouldBe` "((a,b),(a,b))"

  --   it "handles char literals" $ do
  --     p "','" `shouldBe` "','"
  --     p "',', a" `shouldBe` "','"

  --   it "handles string literals" $ do
  --     p [q|","|] `shouldBe` [q|","|]
  --     p [q|",", a|] `shouldBe` [q|","|]

  --   it "handles list literals" $ do
  --     p "[a,b]" `shouldBe` "[a,b]"

  --   it "treats string literals as leaves" $ do
  --     p [q|"(", a|] `shouldBe` [q|"("|]

  --   it "support escaping in leaves" $ do
  --     p [q|"\"", a|] `shouldBe` [q|"\""|]
  --     p [q|"a" ++ "b", a|] `shouldBe` [q|"a" ++ "b"|]

  --   it "misc" $ do
  --     p [q|"((", a|] `shouldBe` [q|"(("|]
  --     p [q|"()(", a|] `shouldBe` [q|"()("|]
  --     p "(a, )" `shouldBe` "(a, )"
  --     p "(, b)" `shouldBe` "(, b)"

  --   it "shouldn't modify the list" $ do
  --     p "[" `shouldBe` "["
  --     p "([)]" `shouldBe` "([)]"
  --     -- p "([)], a" `shouldBe` "([)]"

  --   it "shouldn't modify the list (QuickCheck)" $ property $ let
  --     prop :: String -> Property
  --     prop s = parseExpr s `isPrefixOf` s
  --       $> counterexample [d|show s, parseExpr s|]
  --     in prop

  -- describe "splitOnCommas" $ do
  --   it "should work" $ do
  --     let s = splitOnCommas
  --     s "a" `shouldBe` ["a"]
  --     s "a,b" `shouldBe` ["a", "b"]
  --     s "a, b" `shouldBe` ["a", " b"]
  --     s "(a)" `shouldBe` ["(a)"]
  --     s "(a, b)" `shouldBe` ["(a, b)"]
  --     s "(a, b), b" `shouldBe` ["(a, b)", " b"]
  --     s "','" `shouldBe` ["','"]
  --     s "a ',' b" `shouldBe` ["a ',' b"]
  --     s "(a, b \")\"), b" `shouldBe` ["(a, b \")\")", " b"]

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
  --     [d|1, 2|] `shouldBe` "(1) = 1\t  (2) = 2"
  --     let i = 0.25
  --     let f = (1 -)
  --     [d|i, 1/i, f i, f (1/i)|] `shouldBe`
  --       "(i) = 0.25\t  (1/i) = 4.0\t  (f i) = 0.75\t  (f (1/i)) = -3.0"

  --   it "should step over commas in sub-expressions" $ do
  --     [d|(1, 2), 1|] `shouldBe` "((1, 2)) = (1,2)\t  (1) = 1"
