{-# OPTIONS_GHC -fdefer-type-errors #-}

module Internal.Parser where

import Debug.Trace

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = fff str

fff str = match : splitOnCommas rest
  where
    match = parseExprUntil ',' str
    rest = drop (length match + 1) str

parseExpr :: String -> String
parseExpr = parseExprUntil ','


-- splitAt _ []     = ([], [])
-- splitAt 0 xs     = ([], xs)
-- splitAt n (x:xs) = (x:m, r)
--     where (m, r) = splitAt (n-1) xs

-- groupsOf :: Int -> [a] -> [[a]]
-- groupsOf n [] = []
-- groupsOf n xs = m : groupsOf n r
--     where (m, r) = splitAt n xs

-- groupsOf 2 [1..5]

parseExprUntil :: Char -> String -> (String, String)
parseExprUntil _ "" = ("", "")
parseExprUntil c (x : xs)
  | x == c    = if c == ',' then "" else [x]
  | otherwise =
    x : case x of
      '('  -> recurseUntil ')'
      '['  -> recurseUntil ']'
      '\'' -> leafUntil '\''
      '"'  -> leafUntil '"'
      _    -> parseExprUntil c xs
    where
      recurseUntil = parseWith parseExprUntil
      leafUntil    = parseWith parseLeafUntil
      -- parseWith    = fff' c xs (parseExprUntil c)
      -- fff' c xs thenParse parseUntil cc = match ++ thenParse rest
      parseWith parseUntil cc = match ++ parseExprUntil c rest
        where
          match = parseUntil cc xs
          rest  = drop (length match) xs

parseLeafUntil :: Char -> String -> (String, String)
parseLeafUntil _  "\\" = ("\\", "")
parseLeafUntil _  ""   = ("",   "")
parseLeafUntil cc (x:xs)
  | x == cc   = ([x], xs)
  | x == '\\' = let (y:ys) = xs in
    putMatch x $ putMatch y $ parseLeafUntil cc ys
  | otherwise = putMatch x $ parseLeafUntil cc xs

putMatch :: a -> ([a], [b]) -> ([a], [b])
putMatch x (m, r) = (x:m, r)
