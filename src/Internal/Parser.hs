module Internal.Parser where

import Debug.Trace

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = match : splitOnCommas rest
  where
    match = parseExpr str
    rest = drop (length match + 1) str

parseExpr :: String -> String
parseExpr = parseExprUntil ','

parseExprUntil :: Char -> String -> String
parseExprUntil _ "" = ""
parseExprUntil c (x : xs)
  | x == c      = if c == ',' then "" else [x]
  | x == '('    = x : recurseUntil ')'
  | x == '['    = x : recurseUntil ']'
  | x == '\''   = x : leafUntil '\''
  | x == '"'    = x : leafUntil '"'
  | otherwise   = x : parseExprUntil c xs
    where
      recurseUntil = parseWith parseExprUntil
      leafUntil    = parseWith parseLeafUntil
      -- TODO parseWith parseUntil c xs cc = match ++ parseExprUntil c rest
      parseWith parseUntil cc = match ++ parseExprUntil c rest
        where
          match = parseUntil cc xs
          rest  = drop (length match) xs

parseLeafUntil :: Char -> String -> String
parseLeafUntil _  ""   = ""
parseLeafUntil cc (x:xs)
  | x == cc   = [x]
  | x == '\\' = x : case xs of
    ""       -> ""
    (y:ys)   -> y : parseLeafUntil cc ys
  | otherwise = x : parseLeafUntil cc xs
