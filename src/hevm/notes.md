# Notes

This is a pretty huge PR. Summary of changes:

- rm SBV everywhere
- expand Whiff definition to be a complete over all EVM computations (Expr)
- redefine EVM semantics in terms of the new Expr AST type
- rework hevm storage model to be a global store instead of per contract (makes AST construction much easier)

## TODO

- reimplement existing symexec functionality in terms of queries over the new ast def
- implement all exisiting undefineds to reach feature parity with existing symexec engine
- implement partially symbolic gas model and pure UnexpectedSymbolicArg
- allow knowledge to be attached to AST nodes & implement simplification engine
- implement Expr -> Act decompiler pipeline
