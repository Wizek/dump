import Criterion.Main
import Internal.Parser
import Internal.Utils

main = defaultMain [
  bgroup "parse" $ map (f.(*10000)) [0..4]
  ]
  where
    f n = bench (show n) $ nf splitOnCommas ((dup n "((a,b),(a,b)),") ++ " b")

-- dup :: String -> String
dup n s = concat $ replicate n s
