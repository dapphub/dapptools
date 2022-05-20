# Blade

Blade is an opinionated smt abstraction layer focused on the construction of strongly
typed combinators for the production of smt-lib strings.

Direct access to and control over the raw smt representation greatly simplifies query optimisation
and debugging. For this reason blade focuses on providing facilities that make this approach as safe
and ergonomic as possible.

Queries in blade are constructed by combining fragments of (potentially parameterized) smt2. These
fragments are parsed and (partially) typechecked at compile time, meaning that many kinds of poorly
typed smt generation will result in a compile time error.

Blade currently supports the following theories:

- core
- integer
- bitvector
- uninterpreted functions

The package consists of four core modules:

  - `SMT2.Build`: runtime query generation
  - `SMT2.Exec`: parallel query execution
  - `SMT2.Parse`: compile time smt2 parser
  - `SMT2.Syntax.Typed`: typed smt2 AST

## TODO

- name declaration / ref interpolation
- bv parsing (string into `Nat`??)

## Guarantees

Although blade can detect many kinds of poorly typed smt2 at compile time, it cannot currently catch them
all:

| Error                   | Detection    |
|-------------------------|--------------|
| parse error             | compile time |
| mismatched types        | compile time |
| undeclared dynamic name | compile time |
| undeclared static name  | runtime      |
| duplicated names        | runtime      |

## Usage

### Static Query Generation

Static SMT2 scripts can be created using the quasiquoter interface:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import SMT2.Parse
import SMT2.Syntax.Typed (Script)

script :: Script
script = [smt2|
  (assert (or true (true) false))
  (assert (or true (true) false))
|]
```

Many errors here will result in a ghc type error. For example, the following fragement:

```haskell
script :: Script
script = [smt2|
  (assert (or true (true) false))
  (assert (or 1 (true) false))
|]
```

Produces the following (somewhat helpful) ghc errror:

```
[typecheck] [E] • Exception when trying to run compile-time code:
    Type mismatch. Expected 'Boolean, got 'Integer
CallStack (from HasCallStack):
  error, called at */blade/src/SMT2/Parse.hs:223:15 in main:SMT2.Parse
  Code: Language.Haskell.TH.Quote.quoteExp
          smt2
          "\n\
          \  (assert (or true (true) false))\n\
          \  (assert (or 1 (true) false))\n\
          \  (check-sat)\n"
• In the quasi-quotation:
    [smt2|
  (assert (or true (true) false))
  (assert (or 1 (true) false))
|]
```

## Dynamic Query Generation

Queries can also be constructed at runtime. This is carried out using the `SMT2` monad found in `SMT2.Build`.
Declaring a variable at runtime produces a `Ref` object. These objects can only be constructed using
the `declare` smart constructor in `SMT2.Build`.

TODO: how do we restrict variable referencing?

### Query Execution

Query execution is fully parallelised out of the box. The following executes `query` 100 times
across three instances of Z3:

```haskell
query :: Script
query = [smt2|
  (assert (or true (true) false))
  (assert (and true (true) false))
|]

main :: IO ()
main = withSolvers Z3 3 $ \solvers -> do
  results <- checkSat solvers (replicate 100 query)
  forM_ results (print . snd)
```

### Model Extraction

TODO!

```haskell
main :: IO ()
main = do

```

### Parsing
