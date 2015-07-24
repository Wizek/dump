import Criterion.Main
import Internal.Parser
import Internal.Utils
import Control.DeepSeq (deepseq)
import Data.List

main = defaultMain [
    bgroup "referenceON2" $ map (referenceON2.(*2)) [0..4],
    bgroup "parse" $ map (f.(*2)) [0..4]
  ]
  where
    referenceON2 n = bench (show n) $ xs `deepseq` nf (subsequences) xs
      where xs = (dup n "a") ++ ""

    f n = bench (show n) $ xs `deepseq` nf (splitOnCommas) xs
      where xs = (dup n "a") ++ ""

-- dup :: String -> String
dup n s = concat $ replicate n s
