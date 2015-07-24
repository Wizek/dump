{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Internal.Parser where

import Debug.Trace
-- import Text.Parsec
-- import Text.Parsec.String

splitOnCommas :: String -> [String]
splitOnCommas "" = []
-- splitOnCommas str = fff str

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

parseLeaf :: Char -> String -> [Char] -> (String, String)
parseLeaf _ "\\" _ = ("\\", "")
parseLeaf _ ""   _ = ("",   "")
parseLeaf cc (x:xs) stack
  | x == cc   = withMatch x $ parseExpr xs stack
  | x == '\\' = let (y:ys) = xs in
    withMatch x $ withMatch y $ parseLeaf cc ys stack
  | otherwise = withMatch x $ parseLeaf cc xs stack

withMatch :: a -> ([a], [b]) -> ([a], [b])
withMatch x (m, r) = (x:m, r)

parseExpr :: String -> [Char] -> (String, String)
parseExpr "" _ = ("", "")
parseExpr (',':xs) [] = ("", xs)
parseExpr (x:xs) stack = withMatch x $ case pattern of
  ( _  :_, _:_, True) -> parseExpr xs tack
  ('(' :_, _, _) -> parseExpr xs (')':stack)
  ('[' :_, _, _) -> parseExpr xs (']':stack)
  ('"' :_, _, _) -> parseLeaf '"'  xs stack
  ('\'':_, _, _) -> parseLeaf '\'' xs stack
  _                 -> parseExpr xs stack
  where
    pattern = (x:xs, stack, s == x)
    (_:x2:x2s) = xs
    (s:tack) = stack
