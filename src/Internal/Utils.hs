module Internal.Utils where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6
import Debug.Trace
import Internal.Parser

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

strip  = T.unpack . T.strip . T.pack

wrapInParens :: String -> String
wrapInParens = wrapIn "(" ")"

wrapIn :: String -> String -> String -> String
wrapIn a c b = a ++ b ++ c
