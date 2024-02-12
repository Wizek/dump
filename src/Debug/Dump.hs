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
import Debug.Trace
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
dump = QuasiQuoter
  { quoteExp = process
  , quoteType = \_ -> fl "use in types."
  , quotePat = \_ -> fl "use in patterns."
  , quoteDec = \_ -> fl "generating declarations."
  }
  where
    fl m = fail $ "This quasiquoter is not for " ++ m

-- | Shorthand for `dump`.
d :: QuasiQuoter
d = dump

-- | Shorthand for `dump`.
dd :: QuasiQuoter
dd = dump


newtype HsExp a = HsExp a deriving (Functor, Show, Eq)
unHsExp :: HsExp a -> a
unHsExp (HsExp s) = s

instance Applicative HsExp where
  pure = HsExp
  (HsExp f) <*> (HsExp a) = HsExp $ f a

process :: String -> Q Exp
process = id
  .> \str -> str $> id
  .> removeLineComments
  .> splitOnCommas
  .> filter (fmap Utils.strip .> (/= HsExp ""))
  .> map nameAndValue
  -- .> \x -> traceShow (x,111111111111) x $> id
  .> joinAsColumns
  .> (fmap (++ "\n")) .> wrapInParens
  .> parseHsStrToQQExp str

removeLineComments = id
  .> lines
  .> filter (Utils.strip .> ("-- " `isPrefixOf`) .> not)
  .> unlines

splitOnCommas :: String -> [HsExp String]
splitOnCommas = Parser.splitOnCommas .> map HsExp

nameAndValue :: HsExp String -> HsExp String
nameAndValue = fmap $ \str -> case parseExp str of
  Right (LitE (StringL a)) -> str
  Left e | trace [qc|Debug.Dump: Trouble parsing `{str}`: {e}|] False -> undefined
  _                 -> [qq|"({f str}) = " ++ show ({f2 str})|]
  where
  f2 = Utils.strip
    -- ?????
  f = id
    .> Utils.strip
    .> replace "\"" "\\\""
    .> replace "\n" "\\n\"\n  ++ \""

joinAsColumns :: [HsExp String] -> HsExp String
joinAsColumns = sequenceA .> fmap (intercalate [qc|{nl}  ++ "\t  " ++ |])

wrapInParens :: HsExp String -> HsExp String
wrapInParens = fmap Utils.wrapInParens

parseHsStrToQQExp :: String -> HsExp String -> Q Exp
parseHsStrToQQExp original = id
  .> unHsExp
  .> \s -> s $> id
  .> parseExp
  .> let e =
          [qc|Debug.Dump: parseHsStrToQQExp:{indent 2 original}{indent 10 s})|] in id
  .> let ef = (e ++) .> fail in id
  .> either ef pure

nl = "\n"

data CShow a = CShow (a -> String) a
instance Show (CShow a) where
  show (CShow f a) = f a

indent n str =
  str
  $> lines
  $> map (replicate n ' ' ++)
  $> unlines

-- asd =
--   "aaa \
--   bbb"
