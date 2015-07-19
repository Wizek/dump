module Utils where

import qualified Data.Text as T
import Text.InterpolatedString.Perl6
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Debug.Trace
-- import Text.Parsec.Token

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

strip  = T.unpack . T.strip . T.pack

wrapInParens :: String -> String
wrapInParens = wrapIn "(" ")"

wrapIn :: String -> String -> String -> String
wrapIn a c b = a ++ b ++ c

separate :: String -> [String]
separate s = parse expressions "" s $> either (show .> error) (map strip)

expressions :: Parser [String]
expressions = commaSep expression >>>= eof
  $> ftShow "expressions"

expression :: Parser String
expression =
  (try' $ parens' expression) <|>
  nonComma1
  $> ftShow "expression"

try' = try .> ftShow "try"
parens' = parens .> ftShow "parens"
lookAhead' = lookAhead .> ftShow "lookAhead"

parened = parens

ftShow :: String -> Parser a -> Parser a
ftShow label functor = do
  s <- getInput
  p <- getPosition
  traceM (label ++ " " ++ show p ++ " " ++  show s)
  value <- functor
  traceM (label ++ " matched")
  return value

pTest = parseTest expressions "(a, b)"

nonComma1 :: Parser String
nonComma1 = fmap append nonComma
  $> ftShow "nonComma1"

append :: a -> [a]
append a = [a]

parens :: Parser String -> Parser String
parens = do
  content <- between (char '(') (char ')')
  return content

nonCommas :: Parser String
nonCommas = many nonComma

nonComma :: Parser Char
nonComma = noneOf ","

commaSep :: Parser String -> Parser [String]
commaSep p = p `sepBy` (char ',')

-- separate :: String -> [String]
-- separate = wordsWhen (== ',')

-- -- TODO use parsing to account for [d|1,(2,3)|]
-- wordsWhen     :: (Char -> Bool) -> String -> [String]
-- wordsWhen p s =  case dropWhile p s of
--                       "" -> []
--                       s' -> w : wordsWhen p s''
--                             where (w, s'') = break p s'


a >>>= b = do
  result <- a
  b
  return result
