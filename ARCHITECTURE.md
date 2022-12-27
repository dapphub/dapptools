# Build System

All tools in this directory are built with `nix`, a declarative package manager
which gives a secure and predictable bundling of dependencies.

They are defined as attributes in the `./overlay.nix` file,
which in turn imports the `default.nix` file of each tool.

The dependencies of each tool is set as `buildInputs` in the `default.nix` file.

# dapp & seth

`dapp` and `seth` are similarly structured as a collection of short scripts, mostly written in bash.
The main entrypoint for any invocation of `seth` or `dapp` is a dispatch script,
`./src/seth/libexec/seth/seth` and `./src/dapp/libexec/dapp/dapp` respectively, which parses any
flags given, setting their values to the appropriate environment variable, and dispatches to the
appropriate subcommand.
