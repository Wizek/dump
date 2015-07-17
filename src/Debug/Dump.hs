{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE QuasiQuotes #-}

module Debug.Dump where

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
process str = return $ pairOf str
-- process str = return $ LitE $ StringL ""
  where
    parse :: String -> Exp
    parse = parseExp .> either error id
    pairOf :: String -> Exp
    pairOf str = parse $ [qc|"{str}=" ++ (show ({str}))|]

separate :: String -> [String]
separate = wordsWhen (== ',')

-- TODO use parsing to account for [d|1,(2,3)|]
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

