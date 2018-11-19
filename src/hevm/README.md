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
[hevm v0.11.1 test report]) but we can't guarantee exact
consensus conformance.  The precompiled contracts are entirely missing
for now, and we're in the process of implementing the Metropolis EIPs.

[![asciicast](https://asciinema.org/a/dTPBLV10gixo62ngiSFTK2dVu.png)](https://asciinema.org/a/dTPBLV10gixo62ngiSFTK2dVu)

## Usage

Note: the `hevm` test runner and debugger currently assumes the use of
the `ds-test` framework for Solidity unit tests and the [`dapp` tool
suite](https://github.com/dapphub/dapp).

After running `dapp build`, you can run your unit test suite with

    $ hevm dapp-test

or you can enter the interactive debugger using

    $ hevm interactive

### Debugger key bindings

  - `Esc`: exit debugger
  - `n`: step by one instruction
  - `p`: step backwards by one instruction
  - `N`: step to the next source position
  - `C-n`: step to the next source position and don't enter `CALL` or `CREATE`

## Installing

### Nix

`hevm` is distributed as part of the [Dapp
tools](https://github.com/dapphub/dapptools) suite.

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

```
$ curl -sSL https://get.haskellstack.org/ | sh
$ git clone https://github.com/dapphub/dapptools.git
$ cd dapptools/src/hevm && stack setup && stack install
```

Also, hevm is in Hackage so you can execute `stack install hevm` to
get it up and running.

If you prefer to use your own installation of GHC and the basic
Haskell package manager, Cabal, simply run:

```
$ git clone https://github.com/dapphub/dapptools.git
$ cd dapptools/src/hevm && cabal configure && cabal install
```

**Note:** If you are on macOS when building with
Stack you will have to previously deal with `readline` and
`secp256k1`. These commands should be enough (assuming you
have brew already installed):

```
$ brew install readline # for installing GNU readline in the system
$ stack install readline --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib # for installing the Haskell readline package
$ git clone https://github.com/bitcoin-core/secp256k1.git
$ cd secp256k1
$ ./autogen.sh
$ ./configure --enable-module-recovery # for generating secp256k1_recovery.h
$ make
$ sudo make install
$ cd .. && rm -rf secp256k1 # optional (cleanup)
```

After these commands, you should invoke the installing
commands with the readline flags set:

```
$ stack install --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib # if you are in the cloned repository
$ stack install hevm --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib # if you are installing hevm from Hackage
```

## Contact

You can find us in the DappHub chat at https://dapphub.chat/,
especially the `#dev` channel.

[hevm v0.11.1 test report]: https://hydra.dapp.tools/build/357/download/1/index.html
