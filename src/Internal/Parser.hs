module Internal.Parser where

type Parser = String -> (String, String)

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas expr = m : splitOnCommas r
  where (m, r) = parseExpr expr

parseExpr :: Parser
parseExpr = parseExp' []

parseExp' :: [Char] -> Parser
parseExp' _ "" = ("", "")
parseExp' [] (',':xs) = ("", xs)
parseExp' (s:tack) (x:xs) | x == s = withMatch x $ parseExp' tack xs
parseExp' stack (x:xs) = withMatch x $ case x of
  '('  -> parseExp' (')':stack) xs
  '['  -> parseExp' (']':stack) xs
  '"'  -> parseLeaf (parseExp' stack) '"'  xs
  '\'' -> parseLeaf (parseExp' stack) '\'' xs
  _    -> parseExp' stack xs

parseLeaf :: Parser -> Char -> Parser
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
