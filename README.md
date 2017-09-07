# hsevm [![Build Status](https://travis-ci.org/dapphub/hsevm.svg?branch=master)](https://travis-ci.org/dapphub/hsevm) [![GitHub (pre-)release](https://img.shields.io/github/release/dapphub/hsevm/all.svg)](https://github.com/dapphub/hsevm/releases)

The `hsevm` project is an implementation of the Ethereum virtual
machine (EVM) made specifically for unit testing and debugging smart
contracts.  It is developed by [DappHub](https://github.com/dapphub)
and integrates especially well with the
[`dapp` tool suite](https://github.com/dapphub/dapp).  The `hsevm`
command line program can run unit tests, interactively debug contracts
while showing the Solidity source, or run arbitrary EVM code.

*This software is alpha quality.* It is not yet a 100% complete EVM
(almost everything is implemented, but e.g. the precompiled contracts are missing). Testing against the
Ethereum Foundation's reference test suite is begun but not completed.

[![asciicast](https://asciinema.org/a/5j8eec71hl55l16hsbrn91j8e.png)](https://asciinema.org/a/5j8eec71hl55l16hsbrn91j8e)

## Installing

### Nix

DappHub maintains a repository for the [Nix](https://nixos.org/nix/)
package manager, which works on any Linux distribution, OS X, and
other Unix-likes.

These commands will install Nix, add the DappHub "channel", and
install `hsevm`. The channel has cached binaries for Linux and OS X.

    $ curl https://nixos.org/nix/install | sh
    $ nix-channel --add https://nix.dapphub.com/pkgs/dapphub
    $ nix-channel --update
    $ nix-env -iA dapphub.hsevm

This Nix channel can also be used to install
[`dapp`](https://github.com/dapphub/dapp), our development tool:

    $ nix-env -iA dapphub.dapp

### Static binary

If you don't want to compile anything, and you're on x86-64 Linux, you
can download a static binary from the "Releases" tab on GitHub.
If the static binary complains about a "terminfo" file, you have to
set the `TERMINFO` environment variable; on Ubuntu, you should do

    $ export TERMINFO=/lib/terminfo

(Put that in your `~/.bashrc` for convenience.)

### Building with Stack or Cabal

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
