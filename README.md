# hsevm

The `hsevm` project is an implementation of the Ethereum virtual
machine (EVM) made specifically for unit testing and debugging smart
contracts.  It is developed by [DappHub](https://github.com/dapphub)
and integrates especially well with the
[`dapp` tool suite](https://github.com/dapphub/dapp).  The `hsevm`
command line program can run unit tests, interactively debug contracts
while showing the Solidity source, or run arbitrary EVM code.

*This software is alpha quality.* It is not yet a complete EVM
(especially the precompiled contracts are missing). Testing against the
Ethereum Foundation's reference test suite is begun but not completed.

## Installing

If you don't want to compile anything, and you're on Linux, you can
download a static binary from the "Releases" tab on GitHub.
Otherwise, read on:

The fastest way to build and install `hsevm` is usually to use the
[Nix](https://nixos.org/nix/) package manager, which works on Linux,
OS X, and other Unix-likes.  This should leave you with a working
installation:

    $ curl https://nixos.org/nix/install | sh
    $ git clone https://github.com/dapphub/hsevm
    $ nix-env -f hsevm/hsevm.nix -i hsevm

If you can't or won't use Nix, the easiest way especially if you don't
have GHC (the Haskell compiler) installed already, is to use
[Stack](https://docs.haskellstack.org/en/stable/README/), which can
take care of installing GHC for you.  These commands should work:

    $ curl -sSL https://get.haskellstack.org/ | sh
    $ git clone https://github.com/dapphub/hsevm
    $ cd hsevm && stack setup && stack install

If you prefer to use your own installation of GHC and the basic
Haskell package manager, Cabal, simply run:

    $ git clone https://github.com/dapphub/hsevm
    $ cd hsevm && cabal configure && cabal install

## Running

At this moment, the `hsevm` command line interface is mostly useful
with contracts developed using the
[`dapp` tool suite](https://github.com/dapphub/dapp) along with the
`ds-test` unit testing framework.

After running `dapp build`, you can run your unit test suite with

    $ hsevm dapp-test --json-file ~/my-dapp/out/foo.t.sol.json --dapp-root ~/my-dapp

or you can enter the interactive debugger using

    $ hsevm interactive --json-file ~/my-dapp/out/foo.t.sol.json --dapp-root ~/my-dapp

## Contact

You can find us in the DappHub chat at https://dapphub.chat/,
especially the `#dev` channel.
