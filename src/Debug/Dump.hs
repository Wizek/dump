{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug.Dump where

-- import Data.String.Trim (trim)
import Data.Char (isSpace)
import Data.List (foldl')

import Data.List
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.InterpolatedString.Perl6

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

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
        join = intercalate ([q| ++ ", " ++ |] :: String)
        list :: [String]
        list = separate str
    pairOf :: String -> String
    pairOf str = [qq|"($stripped) = " ++ show ($str)|]
      where
        stripped :: String
        stripped = trim str
    parse :: String -> Exp
    parse = parseExp .> either error id

-- Source: http://stackoverflow.com/a/23040836/499478
trim :: String -> String
trim s = let
  s'    = dropWhile isSpace s
  trim' = foldl'
            (\(c,w) x -> if isSpace x then (c,w+1)
                         else (c+w+1,0)) (0,0) s'
  in
   take (fst trim') s'


wrapInParens :: String -> String
wrapInParens s = [qq|($s)|]

separate :: String -> [String]
separate = wordsWhen (== ',')

-- TODO use parsing to account for [d|1,(2,3)|]
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

