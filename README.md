Example usage:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump

main = print [d|a, a+1, map (+a) [1..3]|]
  where a = 2
```
which prints:

    (a) = 2   (a+1) = 3       (map (+a) [1..3]) = [3,4,5]

by turnint this String

```haskell
"a, a+1, map (+a) [1..3]"
```

into this expression

```haskell
( "(a) = "                ++ show (a)                   ++ "\t  " ++
  "(a+1) = "              ++ show (a + 1)               ++ "\t  " ++
  "(map (+a) [1..3]) = "  ++ show (map (+ a) [1 .. 3])
)
```

# QuickCheck

If you have any intermediate computation within a QuickCheck property, it can
be useful to print a counterexample simply in case of failure

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Debug.Dump
import Test.QuickCheck

($>) = flip ($); infixl 0 $>

prop :: Float -> Property
prop n = n == 1 / (1 / n)
  $> counterexample [d|n, 1/n, 1/(1/n)|]

main = quickCheck prop
```

Which, when executed, will print something like this:

```
*** Failed! Falsifiable (after 15 tests and 133 shrinks):
1.7051912e-38
(n) = 1.7051912e-38   (1/n) = 5.864445e37   (1/(1/n)) = 1.7051913e-38
```

Isn't it nice to see the intermediate computations as well?

See also: [list of features](FEATURES.md)

*Concieved at [this StackOverflow question](http://stackoverflow.com/q/31349556/499478).*
