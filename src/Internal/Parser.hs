module Internal.Parser where

import Debug.Trace

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = match : splitOnCommas rest
  where
    match = pExp str
    rest = drop (length match + 1) str

pExp :: String -> String
pExp = pUntil 0 ','

pUntil :: Int -> Char -> String -> String
pUntil _ _ "" = ""
pUntil i c (x : xs)
  | x == c    = if c == ',' then "" else [x]
  | x == '('  = x : matchAndRest ')'
  | x == '['  = x : matchAndRest ']'
  | x == '\'' = x : leaf '\''
  | x == '"'  = x : leaf '"'
  | otherwise = x : pUntil i c xs
    where
      matchAndRest = factory $ pUntil $ i + 1
      leaf = factory pLeaf
      factory :: (Char -> String -> String) -> Char -> String
      factory f cc = match ++ pUntil i c rest
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
