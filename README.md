Example usage:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump

main = print [d|a, a+1, map (+a) [1..3]|]
  where a = 2
```
which prints:

    (a) = 2, (a+1) = 3, (map (+1) [1..3]) = [2,3,4]


