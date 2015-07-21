module Internal.Parser where

import Debug.Trace

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = match : splitOnCommas rest
  where
    match = pExp str
    rest = drop (length match + 1) str

pExp :: String -> String
pExp = pUntil ','

pUntil :: Char -> String -> String
pUntil _ "" = ""
pUntil c (x : xs)
  | x == c    = if c == ',' then "" else [x]
  | x == '('  = x : matchAndRest ')'
  | x == '['  = x : matchAndRest ']'
  | x == '\'' = x : leaf '\''
  | x == '"'  = x : leaf '"'
  | otherwise = x : pUntil c xs
    where
      matchAndRest = factory pUntil
      leaf = factory pLeaf
      factory :: (Char -> String -> String) -> Char -> String
      factory f cc = match ++ pUntil c rest
        where
          match = f cc xs
          rest = drop (length match) xs

pLeaf :: Char -> String -> String
pLeaf _  ""   = ""
pLeaf cc (x:xs)
  | x == cc   = [x]
  | x == '\\' = x : case xs of
    ""       -> ""
    (y:ys)   -> y : pLeaf cc ys
  | otherwise = x : pLeaf cc xs
