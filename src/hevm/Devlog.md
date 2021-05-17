

# overview

## architecture

The main files are `src/EVM/Expr.hs` which is containing the expression syntax
and `src/EVM/ExprSimp.hs` which contains the simplification/rewrite engine.


## tests

My tests are stored in `xp/test.hs`

they can be build with:

<!-- target: build_tests -->
```sh
cabal new-build xptest
```

<!-- name: build_tests -->
```
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - hevm-0.44.1 (test:xptest) (file xp/test.hs changed)
Preprocessing test suite 'xptest' for hevm-0.44.1..
Building test suite 'xptest' for hevm-0.44.1..
Linking /home/mhhf/src/dapptools/src/hevm/dist-newstyle/build/x86_64-linux/ghc-8.8.4/hevm-0.44.1/t/xptest/build/xptest/xptest ...
```


<!-- target: testing -->
```sh
./xptest
```

<!-- name: testing  -->
```
Tests
  Properties
    toExpr . fromExpr: OK (0.02s)
      +++ OK, passed 100 tests.
    parse . show:      OK (0.03s)
      +++ OK, passed 100 tests.

All 2 tests passed (0.06s)
```
