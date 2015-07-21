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

import qualified Internal.Utils as Utils
import Internal.Utils (($>), (.>))
import Data.List
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.InterpolatedString.Perl6

newtype Stripped = Stripped { unStripped :: String }
newtype HsExpStr = HsExpStr { unHsExpStr :: String }
newtype NameAndValue = NameAndValue { unNameAndValue :: String }

instance Show Stripped where show = unStripped
instance Show HsExpStr where show = unHsExpStr
instance Show NameAndValue where show = unNameAndValue

-- | This is the main `QuasiQuoter`.
dump :: QuasiQuoter
dump = QuasiQuoter {quoteExp = process}

-- | Shorthand for `dump`.
d = dump

-- | Shorthand for `dump`.
dd = dump


process :: String -> Q Exp
process = id
  .> splitOnCommas
  .> map nameAndValue
  .> join
  .> wrapInParens
  .> unHsExpStr
  .> strToExp
  .> return

splitOnCommas :: String -> [HsExpStr]
splitOnCommas = Utils.separate .> map HsExpStr

nameAndValue :: HsExpStr -> HsExpStr
nameAndValue (HsExpStr str) = [qq|"({Utils.strip str}) = " ++ show ($str)|] $> HsExpStr

join :: [HsExpStr] -> HsExpStr
join = map unHsExpStr .> intercalate [q| ++ "\t  " ++ |] .> HsExpStr

wrapInParens :: HsExpStr -> HsExpStr
wrapInParens = unHsExpStr .> Utils.wrapInParens .> HsExpStr

strToExp :: String -> Exp
strToExp = parseExp .> either error id


