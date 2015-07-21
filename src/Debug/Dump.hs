{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-|

`d`, `dd`, and `dump` are aliases of the same `QuasiQuoter`. you can choose to
imort just one of them:

@
import Debug.Dump (dd)
@

Example usage:

@
{&#45;\# LANGUAGE QuasiQuotes \#&#45;}

import Debug.Dump

main = print [d|a, a+1, map (+a) [1..3]|]
  where a = 2
@

which prints:

@
(a) = 2   (a+1) = 3       (map (+a) [1..3]) = [3,4,5]
@

by turnint this String

@
"a, a+1, map (+a) [1..3]"
@

into this expression

@
( "(a) = " ++ show (a)            ++ "\t  " ++
  "(a+1) = " ++ show (a + 1)      ++ "\t  " ++
  "(map (+a) [1..3]) = " ++ show (map (+ a) [1 .. 3])
)
@

-}

module Debug.Dump (d, dd, dump) where

import Internal.Utils
import Data.List
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.InterpolatedString.Perl6


-- | This is the main `QuasiQuoter`.
dump :: QuasiQuoter
dump = QuasiQuoter {quoteExp = process}

-- | Shorthand for `dump`.
d = dump

-- | Shorthand for `dump`.
dd = dump

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


