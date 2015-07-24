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

parseExpr :: [Char] -> String -> (String, String)
parseExpr _ "" = ("", "")
parseExpr [] (',':xs) = ("", xs)
parseExpr (s:tack) (x:xs) | x == s = withMatch x $ parseExpr tack xs
parseExpr stack (x:xs) = withMatch x $ case x of
  '('  -> parseExpr (')':stack) xs
  '['  -> parseExpr (']':stack) xs
  '"'  -> parseLeaf '"'  xs stack
  '\'' -> parseLeaf '\'' xs stack
  _    -> parseExpr stack xs

parseLeaf :: Char -> String -> [Char] -> (String, String)
parseLeaf _ "\\" _ = ("\\", "")
parseLeaf _ ""   _ = ("",   "")
parseLeaf cc (x:xs) stack
  | x == cc   = withMatch x $ parseExpr stack xs
  | x == '\\' = let (y:ys) = xs in
    withMatch x $ withMatch y $ parseLeaf cc ys stack
  | otherwise = withMatch x $ parseLeaf cc xs stack

withMatch :: a -> ([a], [b]) -> ([a], [b])
withMatch x (m, r) = (x:m, r)
