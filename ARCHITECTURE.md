All tools in this directory are built with `nix`, a declarative package manager 
which gives a secure and predictable bundling of dependencies.

They are defined as attributes in the `./overlay.nix` file, 
which in turn imports the `default.nix` file of each tool.

The dependencies of each tool is set as `buildInputs` in the `default.nix` file.

`dapp` and `seth` are similarly structured as a collection of short scripts,
mostly written in bash. The main entrypoint for any invocation of `seth` or `dapp`
is a dispatch script, `./src/seth/libexec/seth/seth` and `./src/dapp/libexec/dapp/dapp` respectively, which parses any flags given, setting their values to the appropriate environment variable,
and dispatches to the appropriate subcommand.

Much of the heavy lifting in `dapptools` is done in `hevm`. To get a feel of how it is structured,
a good starting point is the `exec1` function of `/src/hevm/src/EVM.hs`, which executes the `vm` one opcode in the monad `type EVM a = State VM a`.

If we need to perform `IO` while executing, for example to perform an rpc call or an smt query,
execution stops with a `VMFailure (Query _)`, which is handled in `Fetch.hs`.

This exchange between execution and handling side effects is guided by an operational monad pattern, which can be seen as defining a set of "big step opcodes", which are interpreted differently depending on the context.

For example, the interpreter in `TTY.hs` asks for user input when reaching a branching point during symbolic execution, while the interpreter in `SymExec.hs` performs a depth-first traversal of all paths.

Symbolic execution relies heavily on the `sbv` library, although that may come to change in the future.
