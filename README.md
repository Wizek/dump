Example usage:

    {-# LANGUAGE QuasiQuotes #-}

    import Debug.Dump

    main = print [d|a, a+1, map (+a) [1..3]|]
      where a = 2




