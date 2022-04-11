# Blade

Blade is an opinionated smt2 abstraction layer focused on the construction of strongly
typed combinators for both the consumption and production of smt2-lib strings.

Blade takes the view that direct access to and control over the raw smt representation greatly
simplifies query optimisation and debugging. For this reason blade focuses on providing facilities
that make this approach as safe and ergonomic as possible.

Queries in blade are constructed by combining fragments of (potentially parameterized) smt2.
These fragments are parsed and typechecked at compile time and used to direct typesystem
analysis that is able to statically ensure that all kinds of poorly typed smt generation or
faulty model parsing will result in a compile time error.

The package consists of five submodules:

  - SMT2: query generation framework
  - Exec: parallel by default query executor
  - Type: compile time smt2 typechecker
  - Read: library of smt2 parser combinators
  - Show: counterexample pretty printing framework

## Usage

### Query Generation

```haskell
-- This produces compile time errors due to the smt2 type errors.
broken :: SMT2 e
broken
  = [smt2|
      (declare-const a Int)
      (declare-const a Bool)
      (assert c)
    |]

-- Fragments can be parameterized.
param :: Int -> SMT2 e
param bVal
  = [smt2|
      (declare-const a Int)
      (define-const b Int ${bVal})
      (assert (= a b))
    |]

-- Fragments can use typeclass constraints to refer to undeclared variables.
-- Failure to abide by this contract will produce a compile time error.
partial :: (Has "a" SBool e, Has "b" SInt e) => SMT2 e
partial
  = [smt2|
      (assert (and a (lt b 10)))
      (check-sat)
    |]

-- Variables can be declared at runtime. Although we cannot statically ensure that these names are
-- well typed, we provide runtime mechanisms that are constrained in ways that allow the typechecker to
-- infer well-typededness in other combinators
dyndec :: [String] -> SMT2 e
dyndec names = do
  vs <- forM names (declare . fresh)
  query = forM vs (assert v)

-- Fragments can easily be sequenced and combined
combined :: [String] -> Int -> SMT2 e
combined names v = do
  dyndec names
  declare @"a" SBool
  declare @"b" SInt
  partial
```

### Query Execution

```haskell
main :: IO ()
main = do
  -- produce a list of 100 smt queries using the above function
  let qs = fmap query [1..100]

  -- spawn 5 solver instances and run the queries in parallel
  solvers <- spawnSolvers Bitwuzla 5
  results <- runQueries solvers qs
```

### Model Extraction

```haskell
main :: IO ()
main = do

```

### Parsing
