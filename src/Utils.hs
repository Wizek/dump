module Utils where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6
import Debug.Trace
import Internal.Parse

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

strip  = T.unpack . T.strip . T.pack

wrapInParens :: String -> String
wrapInParens = wrapIn "(" ")"

wrapIn :: String -> String -> String -> String
wrapIn a c b = a ++ b ++ c

separate :: String -> [String]
separate = parseSimple
-- separate = wordsWhen (== ',')

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
