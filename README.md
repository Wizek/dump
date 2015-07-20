Example usage:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump

main = print [d|a, a+1, map (+a) [1..3]|]
  where a = 2
```
which prints:

    (a) = 2   (a+1) = 3       (map (+a) [1..3]) = [3,4,5]

Have a look at the [list of features](FEATURES.md).

*Concieved at [this StackOverflow question](http://stackoverflow.com/q/31349556/499478).*
