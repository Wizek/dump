{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Internal.Parser where

import Debug.Trace
-- import Text.Parsec
-- import Text.Parsec.String

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas str = fff str

-- fff str = match : splitOnCommas rest
--   where
--     match = parseExprUntil ',' str
--     rest = drop (length match + 1) str

-- parseExpr :: String -> String
-- parseExpr = parseExprUntil ','


-- splitAt _ []     = ([], [])
-- splitAt 0 xs     = ([], xs)
-- splitAt n (x:xs) = (x:m, r)
--     where (m, r) = splitAt (n-1) xs

-- groupsOf :: Int -> [a] -> [[a]]
-- groupsOf n [] = []
-- groupsOf n xs = m : groupsOf n r
--     where (m, r) = splitAt n xs

-- groupsOf 2 [1..5]

-- parseLeaf = do
--   noneOf ","

-- parseLeafUntil :: Char -> String -> (String, String)
-- parseLeafUntil _  "\\" = ("\\", "")
-- parseLeafUntil _  ""   = ("",   "")
-- parseLeafUntil cc (x:xs)
--   | x == cc   = ([x], xs)
--   | x == '\\' = let (y:ys) = xs in
--     putMatch x $ putMatch y $ parseLeafUntil cc ys
--   | otherwise = putMatch x $ parseLeafUntil cc xs

putMatch :: a -> ([a], [b]) -> ([a], [b])
putMatch x (m, r) = (x:m, r)

fff "" _ = ("", "")
fff (x:xs) stack = case (x, stack, head stack == x) of
  (_, _:_, True) -> putMatch x $ fff xs (tail stack)
  (',', [], _) -> ("", xs)
  ('(', _, _) -> putMatch x $ fff xs (')':stack)
  ('[', _, _) -> putMatch x $ fff xs (']':stack)
  _   -> putMatch x $ fff xs stack
