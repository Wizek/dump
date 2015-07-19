Example usage:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump

main = print [d|a, a+1, map (+a) [1..3]|]
  where a = 2
```
which prints:

    (a) = 2   (a+1) = 3       (map (+a) [1..3]) = [3,4,5]


