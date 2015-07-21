{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveFunctor #-}

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

import qualified Internal.Utils as U
import Internal.Utils (($>), (.>))
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


newtype HsExp a = HsExp a deriving (Functor)
unHsExp (HsExp s) = s

process :: String -> Q Exp
process = id
  .> splitOnCommas
  .> map nameAndValue
  .> joinAsColumns
  .> wrapInParens
  .> parseHsStrToQQExp
  .> return

splitOnCommas :: String -> [HsExp String]
splitOnCommas = U.separate .> map HsExp

nameAndValue :: HsExp String -> HsExp String
nameAndValue = fmap $ \str-> [qq|"({U.strip str}) = " ++ show ($str)|]

joinAsColumns :: [HsExp String] -> HsExp String
joinAsColumns = map unHsExp .> intercalate [q| ++ "\t  " ++ |] .> HsExp

wrapInParens :: HsExp String -> HsExp String
wrapInParens = fmap U.wrapInParens

parseHsStrToQQExp :: HsExp String -> Exp
parseHsStrToQQExp = unHsExp .> parseExp .> either error id


