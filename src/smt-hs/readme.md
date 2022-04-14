# Blade

Blade is an opinionated smt2 abstraction layer focused on the construction of strongly
typed combinators for both the consumption and production of smt2-lib strings.

Direct access to and control over the raw smt representation greatly simplifies query optimisation
and debugging. For this reason blade focuses on providing facilities that make this approach as safe
and ergonomic as possible.

Queries in blade are constructed by combining fragments of (potentially parameterized) smt2.
These fragments are parsed and typechecked at compile time and used to direct typesystem
analysis that is able to statically ensure that all kinds of poorly typed smt generation will result
in a compile time error.

The package consists of five submodules:

  - `SMT2`: query generation framework
  - `Exec`: parallel by default query executor
  - `Type`: compile time smt2 typechecker
  - `Read`: library of smt2 parser combinators
  - `Show`: counterexample pretty printing framework

## Usage

### Query Generation

Well typedenss of fully static smt can be enforced with compile time checks only. The following
produces compile time errors due to the smt2 type errors.

```haskell
broken :: SMT2 e
broken
  = [smt2|
      (declare-const a Int)
      (declare-const a Bool)
      (assert c)
    |]
```

These guarantees extend to fragments where all names and types are known, but constants are
parameterized:

```haskell
param :: Int -> SMT2 e
param bVal
  = [smt2|
      (declare-const a Int)
      (define-const b Int ${bVal})
      (assert (= a b))
    |]
```

Fragments can use typeclass constraints to refer to undeclared variables.
Failure to abide by this contract will produce a compile time error.

```haskell
partial :: (Has "a" SBool e, Has "b" SInt e) => SMT2 e
partial
  = [smt2|
      (assert (and a (lt b 10)))
      (check-sat)
    |]
```

Variables can be declared at runtime.

We cannot provide any useful guarantees of name freshness for runtime declared variables, so
delegate this responsibility to the user and accept the possibility of badly typed SMT generation in
this case.

We can however statically ensure that all references are to a declared variable of the correct time.
In order to do this we have to introduce some runtime machinery:

- A runtime copy of the typing environment
- Inclusion proofs for the typing environment

We then only allow variables to be declared at runtime if the caller can provide a proof of
inclusion. Since the only way to produce such a proof is to declare a variable beforehand, we have a
guarantee that all referenced names in the generated smt2 are present in the typing context with a
matching type.

The monad instance of the `SMT2` type automates the management of the typechecking environment and
provides convenient interfaces for working with the proofs. There are monadic smart constructors for
the top level commands.

```haskell
dyndec :: [String] -> SMT2 e
dyndec names = do
  -- this returns an array of proofs of inclusion
  -- note we do not make any freshness checks here!
  vs <- mapM declare SBool names

  -- assert each declared variable & check sat
  mapM assert vs
  checkSat
```

Both static and dynamic fragments can easily be sequenced and combined.

```haskell
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
