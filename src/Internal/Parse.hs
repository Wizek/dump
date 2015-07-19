module Internal.Parse where

-- import Text.InterpolatedString.Perl6
-- import Text.Parsec
-- import Text.Parsec.String
-- import Text.Parsec.Char
import Debug.Dump
import Debug.Trace

-- parseSimple :: String -> [String]
-- parseSimple (c:rest)
--   | c == ',' = [parseSimple rest]

pExp' :: String -> String  -> (String, String)
pExp' soFar "" = (soFar, "")
pExp' soFar (c : rest)
  | c == ',' = (soFar, rest)
  -- | c == '(' = (soFar, rest)
  | c /= ',' = pExp' (soFar ++ [c]) rest

pExp :: String -> String
pExp = pUntil 0 ','

-- pUntil :: Char -> String -> String
pUntil :: Int -> Char -> String -> String
pUntil _ _ "" = ""
pUntil i c (x : xs)
  -- | trace (show (c, i, x:xs)) False = undefined
  -- | trace ("c=" ++ show c ++ "i=" ++ show i ++ "x:xs=" ++ show (x:xs)) False = undefined
  -- | trace [d|c, i, x:xs|] False = undefined
  | x == c    = ""
  | x == '('  = x : matchAndRest ')'
  | x == '['  = x : matchAndRest ']'
  | x == '\'' = x : leaf '\''
  | x == '"'  = x : leaf '"'
  | otherwise = x : pUntil i c xs
    where
      matchAndRest (cc @ closingChar)
        = match ++ [cc] ++ pUntil i c rest
        where
          match = pUntil (i+1) cc xs
          rest = drop (length match + 1) xs
      -- leaf cc = takeWhile (/= cc) xs ++ [cc]
      leaf cc = pLeaf cc xs ++ [cc]

pLeaf :: Char -> String -> String
-- pLeaf cc xs
--   | trace [d|cc, xs|] False = undefined
pLeaf _  ""   = ""
pLeaf cc (x:xs)
  | x == cc   = ""
  | x == '\\' = x : case xs of
    ""       -> ""
    (y:ys)   -> y : pLeaf cc ys
  | otherwise = x : pLeaf cc xs

-- if I see a shash, ignore the next character
