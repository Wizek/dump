module Internal.Parser where

type Parser = String -> (String, String)

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas expr = let (m, r) = parseExpr expr in m : splitOnCommas r

parseExpr :: Parser
parseExpr = parseExpr' []

parseExpr' :: [Char] -> Parser
parseExpr' _ "" = ("", "")
parseExpr' [] (',':xs) = ("", xs)
parseExpr' (s:tack) (x:xs) | x == s = withMatch x $ parseExpr' tack xs
parseExpr' stack (x:xs) = withMatch x $ case x of
  '('  -> parseExpr' (')':stack) xs
  '['  -> parseExpr' (']':stack) xs
  '"'  -> parseLeaf (parseExpr' stack) '"'  xs
  '\'' -> parseLeaf (parseExpr' stack) '\'' xs
  _    -> parseExpr' stack xs

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
