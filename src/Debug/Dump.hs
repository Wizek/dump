{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Debug.Dump (d, dump) where

import Utils
import Data.List
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.InterpolatedString.Perl6

dump :: QuasiQuoter
dump = QuasiQuoter {quoteExp = process}

d = dump

process :: String -> Q Exp
process str = pairsOf str $> parse $> return
  where
    pairsOf :: String -> String
    pairsOf str = join (map pairOf list) $> wrapInParens
      where
        join :: [String] -> String
        join = intercalate ([q| ++ "\t  " ++ |] :: String)
        list :: [String]
        list = separate str
    pairOf :: String -> String
    pairOf str = [qq|"($stripped) = " ++ show ($str)|]
      where
        stripped :: String
        stripped = strip str
    parse :: String -> Exp
    parse = parseExp .> either error id


