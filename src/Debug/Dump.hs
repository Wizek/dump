module Debug.Dump where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

dump :: QuasiQuoter
dump = QuasiQuoter {quoteExp = process}

d = dump

process :: String -> Q Exp
process _ = return $ LitE $ StringL ""
