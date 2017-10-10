# hevm [![Build Status](https://travis-ci.org/dapphub/hevm.svg?branch=master)](https://travis-ci.org/dapphub/hevm) [![GitHub (pre-)release](https://img.shields.io/github/release/dapphub/hevm/all.svg)](https://github.com/dapphub/hevm/releases)

The `hevm` project is an implementation of the Ethereum virtual
machine (EVM) made specifically for unit testing and debugging smart
contracts.  It is developed by [DappHub](https://github.com/dapphub)
and integrates especially well with the
[`dapp` tool suite](https://github.com/dapphub/dapp).  The `hevm`
command line program can run unit tests, interactively debug contracts
while showing the Solidity source, or run arbitrary EVM code.

*This is not yet a complete EVM*.  For example, the precompiled
contracts are missing.  We do well on Ethereum's `VMTests` suite (see
[hevm v0.10 test report]) but we can't guarantee exact
consensus conformance.

[![asciicast](https://asciinema.org/a/5j8eec71hl55l16hsbrn91j8e.png)](https://asciinema.org/a/5j8eec71hl55l16hsbrn91j8e)

## Installing

### Nix

DappHub maintains a repository for the [Nix](https://nixos.org/nix/)
package manager, which works on any Linux distribution, OS X, and
other Unix-likes.

These commands will install Nix, add the DappHub "channel", and
install `hevm`. The channel has cached binaries for Linux and OS X.

    $ curl https://nixos.org/nix/install | sh
    $ nix-channel --add https://nix.dapphub.com/pkgs/dapphub
    $ nix-channel --update
    $ nix-env -iA dapphub.hevm

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
    $ git clone https://github.com/dapphub/hevm
    $ cd hevm && stack setup && stack install

If you prefer to use your own installation of GHC and the basic
Haskell package manager, Cabal, simply run:

    $ git clone https://github.com/dapphub/hevm
    $ cd hevm && cabal configure && cabal install

## Running

At this moment, the `hevm` command line interface is mostly useful
with contracts developed using the
[`dapp` tool suite](https://github.com/dapphub/dapp) along with the
`ds-test` unit testing framework.

After running `dapp build`, you can run your unit test suite with

    $ hevm dapp-test

or you can enter the interactive debugger using

    $ hevm interactive

## Contact

You can find us in the DappHub chat at https://dapphub.chat/,
especially the `#dev` channel.

[hevm v0.10 test report]: https://hydra.dapp.tools/build/183/download/1/index.html
