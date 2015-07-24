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
  '"'  -> parseLeaf (parseExpr stack) '"'  xs
  '\'' -> parseLeaf (parseExpr stack) '\'' xs
  _    -> parseExpr stack xs

parseLeaf :: (String -> (String, String)) -> Char -> String -> (String, String)
parseLeaf _ _ "\\" = ("\\", "")
parseLeaf _ _ ""   = ("",   "")
parseLeaf cont cc (x:xs) = withMatch x result
  where
  result
    | x == cc   = cont xs
    | x == '\\' = withMatch y $ parseLeaf cont cc ys
    | otherwise = parseLeaf cont cc xs
  (y:ys) = xs

withMatch :: a -> ([a], [b]) -> ([a], [b])
withMatch x (m, r) = (x:m, r)
