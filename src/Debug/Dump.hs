{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveFunctor #-}

{-|

`d`, `dd`, and `dump` are aliases of the same `QuasiQuoter`. You can choose to
import just one of them if you want:

For example usage, see README.md, e.g. on
[GitHub](https://github.com/Wizek/dump#readme) or on
[Hackage](http://hackage.haskell.org/package/dump#readme):

-}

module Debug.Dump (d, dd, dump) where

import Data.List
import Data.List.Utils
import Data.Traversable
import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.Meta.Parse
import Text.InterpolatedString.Perl6

import Internal.Utils (($>), (.>))
import qualified Internal.Utils as Utils
import qualified Internal.Parser as Parser

-- | This is the main `QuasiQuoter`.
dump :: QuasiQuoter
dump = QuasiQuoter {quoteExp = process}

-- | Shorthand for `dump`.
d :: QuasiQuoter
d = dump

-- | Shorthand for `dump`.
dd :: QuasiQuoter
dd = dump


newtype HsExp a = HsExp a deriving (Functor)
unHsExp :: HsExp a -> a
unHsExp (HsExp s) = s

instance Applicative HsExp where
  pure = HsExp
  (HsExp f) <*> (HsExp a) = HsExp $ f a

process :: String -> Q Exp
process = id
  .> splitOnCommas
  .> map nameAndValue
  .> joinAsColumns
  .> wrapInParens
  .> parseHsStrToQQExp
  .> return

splitOnCommas :: String -> [HsExp String]
splitOnCommas = Parser.splitOnCommas .> map HsExp

nameAndValue :: HsExp String -> HsExp String
nameAndValue = fmap $ \str-> [qq|"({f str}) = " ++ show ($str)|]
  where
  f str = replace "\"" "\\\"" $ Utils.strip str

joinAsColumns :: [HsExp String] -> HsExp String
joinAsColumns = sequenceA .> fmap (intercalate [q| ++ "\t  " ++ |])

wrapInParens :: HsExp String -> HsExp String
wrapInParens = fmap Utils.wrapInParens

parseHsStrToQQExp :: HsExp String -> Exp
parseHsStrToQQExp = unHsExp .> parseExp .> either error id

