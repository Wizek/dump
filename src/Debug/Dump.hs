{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-|

`d`, `dd`, and `dump` are aliases of the same `QuasiQuoter`. you can choose to
imort just one of them:

@
import Debug.Dump (dd)
@

-}

module Debug.Dump (d, dd, dump) where

import Utils
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


