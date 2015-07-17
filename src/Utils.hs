module Utils where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6

strip  = T.unpack . T.strip . T.pack

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

