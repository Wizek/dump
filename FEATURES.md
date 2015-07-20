# Description

To attain a changelog of this library between two arbitrary versions
you can use `git diff`. For example: `git diff v0.1.0..v0.2.0 -- FEATURES.md`

# Features

* Use it with Debug.Trace: `trace [d|x, y, z|] x`
* Use it with Test.QuickCheck: `counterexample [d|x, y, z|]`
* Use it inside the IO monad: `putStrLn [d|x, y, z|]`

* print out a variable `let a = 1 in print [d|a|]` outputing: `(a) = 1`
* print out an expression `print [d|1 + 2|]` outputting: `(1 + 2) = 3`
* refer to variables in the expression `let a = [1..3] in print [d|map (+1) a|]`
  outputing: `(map (+1) a) = [2,3,4]`
* print out multiple expressions `[d|1+2, 3+4|]` -> "(1+2) = 3   (3+4) = 7"
* tries a bit to line up expressions in columns.

  ```haskell
  main = do
    f 22
    f 333
    where f a = putStrLn [d|a, a+2|]
  ```
  Outputs:

        (a) = 22          (a+2) = 24
        (a) = 333         (a+2) = 335

# Beta features

* Support for expressions that contain commas inside them:
  An example: `[d|1, [2, 3]|]` -> "(1) = 1   ([2, 3]) = [2,3]"

  If your expression is invalid Haskell, such as `[d| ([)] |]` the generated
  code will be invalid as well, but could be slightly transformed. If you get
  cryptic error messages because of this, please let us know by opening
  an issue on GitHub.

# Known issues

n/a
