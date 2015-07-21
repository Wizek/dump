module Internal.Parser where

import Debug.Trace

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = match : splitOnCommas rest
  where
    match = parseExp str
    rest = drop (length match + 1) str

parseExp :: String -> String
parseExp = parseExpUntil ','

parseExpUntil :: Char -> String -> String
parseExpUntil _ "" = ""
parseExpUntil c (x : xs)
  | x == c    = if c == ',' then "" else [x]
  | x == '('  = x : recurse ')'
  | x == '['  = x : recurse ']'
  | x == '\'' = x : leaf '\''
  | x == '"'  = x : leaf '"'
  | otherwise = x : parseExpUntil c xs
    where
      recurse = factory parseExpUntil
      leaf = factory parseLeaf
      factory :: (Char -> String -> String) -> Char -> String
      factory parse cc = match ++ parseExpUntil c rest
        where
          match = parse cc xs
          rest = drop (length match) xs

parseLeaf :: Char -> String -> String
parseLeaf _  ""   = ""
parseLeaf cc (x:xs)
  | x == cc   = [x]
  | x == '\\' = x : case xs of
    ""       -> ""
    (y:ys)   -> y : parseLeaf cc ys
  | otherwise = x : parseLeaf cc xs
