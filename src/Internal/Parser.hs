module Internal.Parser where

import qualified  Data.List             as List

type Parser = String -> (String, String)

splitOnCommas :: String -> [String]
splitOnCommas "" = []
splitOnCommas expr = m : splitOnCommas r
  where (m, r) = parseExpr expr

parseExpr :: Parser
parseExpr = parseExp' []

parseExp' :: [String] -> Parser
parseExp' _ "" = ("", "")
parseExp' [] (',':xs) = ("", xs)
parseExp' (s:tack) str@(x:xs)
    | Just rest <- sp s str = withMatch x $ parseExp' tack rest
parseExp' stack str@(x:xs) = withMatch x f
  where
  f
    | Just rest <- sp "\"" str = parseLeaf (parseExp' stack) "\""  rest
    | Just rest <- sp "\'" str = parseLeaf (parseExp' stack) "\'"  rest
    | Just rest <- sp "{-" str = withMatch '-' $ parseLeaf (parseExp' stack) "-}"  rest
    | Just rest <- sp "("  str = parseExp' (")":stack) rest
    | Just rest <- sp "["  str = parseExp' ("]":stack) rest
    | Just rest <- sp "{"  str = parseExp' ("}":stack) rest
    | otherwise                = parseExp' stack xs

parseLeaf :: Parser -> String -> Parser
parseLeaf _ _ "\\" = ("\\", "")
parseLeaf _ _ ""   = ("",   "")
parseLeaf cont cc str@(x:xs) = withMatch x result
  where
  result
    -- | cc `isPrefixOf` rest = cont xs
    -- | Just rest <- sp cc str = (case cc of _:a:"" -> withMatch a; _ -> id) $ cont rest
    | Just rest <- sp cc str = withMatches (drop 1 cc) $ cont rest
    | x == '\\' = withMatch y $ parseLeaf cont cc ys
    | otherwise = parseLeaf cont cc xs
  (y:ys) = xs

sp = List.stripPrefix

withMatch :: a -> ([a], [b]) -> ([a], [b])
withMatch x (m, r) = (x:m, r)

withMatches :: [a] -> ([a], [b]) -> ([a], [b])
withMatches x (m, r) = (x ++ m, r)
