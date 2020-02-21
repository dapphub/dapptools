# hevm [![Build Status](https://travis-ci.com/dapphub/dapptools.svg?branch=master)](https://travis-ci.com/dapphub/dapptools)

The `hevm` project is an implementation of the Ethereum virtual machine (EVM) made specifically for unit testing and debugging smart contracts.  It is developed by [DappHub](https://github.com/dapphub) and integrates especially well with the [`dapp` tool suite](https://github.com/dapphub/dapp). The `hevm` command line program can run unit tests, interactively debug contracts while showing the Solidity source, or run arbitrary EVM code.

### Usage

Note: the `hevm` test runner and debugger currently assumes the use of the `ds-test` framework for Solidity unit tests and the [`dapp` tool suite](https://github.com/dapphub/dapp).

After running `dapp build`, you can run your unit test suite with

    $ hevm dapp-test

or you can enter the interactive debugger using

    $ hevm interactive

### Commands

    hevm -- Ethereum evaluator

    Usage: hevm [<options>] <command>
       or: hevm <command> --help

    Commands:

      exec         Execute a given program with specified env & calldata
      dapp-test    Run unit tests
      interactive  Browse and run unit tests interactively
      flatten      Concat all dependencies for a given source file

      bc-test      Run an Ethereum Blockchain/GeneralState test
      vm-test      Run an Ethereum VMTest
      compliance   Run Blockchain or VMTest compliance report

      emacs        Emacs console
      version      Show hevm version

### Interactive debugger key bindings

  - `Esc`: exit debugger
  - `a`: step to start
  - `e`: step to end
  - `n`: step forwards by one instruction
  - `p`: step backwards by one instruction
  - `N`: step to the next source position
  - `C-n`: step to the next source position and don't enter `CALL` or `CREATE`
  - `m`: toggle memory view
  - `h`: show key-binding help

### Environment Variables 

These environment variables can be used to control block parameters

  - `DAPP_TEST_ADDRESS`
  - `DAPP_TEST_CALLER`
  - `DAPP_TEST_ORIGIN`
  - `DAPP_TEST_GAS_CREATE`
  - `DAPP_TEST_GAS_CALL`
  - `DAPP_TEST_BALANCE_CREATE`
  - `DAPP_TEST_BALANCE_CALL`
  - `DAPP_TEST_COINBASE`
  - `DAPP_TEST_NUMBER`
  - `DAPP_TEST_TIMESTAMP`
  - `DAPP_TEST_GAS_LIMIT`
  - `DAPP_TEST_GAS_PRICE`
  - `DAPP_TEST_DIFFICULTY`

## Installing

### Nix

`hevm` is distributed as part of the [Dapp tools](https://github.com/dapphub/dapptools) suite.

### Static binary

If you don't want to compile anything, and you're on x86-64 Linux, you can download a static binary from the "Releases" tab on GitHub. If the static binary complains about a "terminfo" file, you have to set the `TERMINFO` environment variable; on Ubuntu, you should do

    $ export TERMINFO=/lib/terminfo

(Put that in your `~/.bashrc` for convenience.)

### Building with Stack or Cabal

If you can't or won't use Nix, the easiest way especially if you don't have GHC (the Haskell compiler) installed already, is to use [Stack](https://docs.haskellstack.org/en/stable/README/), which can take care of installing GHC for you. These commands should work:

    $ curl -sSL https://get.haskellstack.org/ | sh
    $ git clone https://github.com/dapphub/dapptools.git
    $ cd dapptools/src/hevm && stack setup && stack install

Also, hevm is in Hackage so you can execute `stack install hevm` to get it up and running.

If you prefer to use your own installation of GHC and the basic Haskell package manager, Cabal, simply run:

    $ git clone https://github.com/dapphub/dapptools.git
    $ cd dapptools/src/hevm && cabal configure && cabal install

**Note:** If you are on macOS when building with Stack, you will first need to install the [secp256k1](https://github.com/bitcoin-core/secp256k1) and [libff](https://github.com/scipr-lab/libff) libraries. These commands should be enough:

    $ git clone https://github.com/bitcoin-core/secp256k1.git
    $ cd secp256k1
    $ ./autogen.sh
    $ ./configure --enable-module-recovery # for generating secp256k1_recovery.h
    $ make
    $ sudo make install
    $ cd .. && rm -rf secp256k1 # optional (cleanup)

    $ git clone https://github.com/scipr-lab/libff --recursive
    $ cd libff
    $ export LDFLAGS=-L/usr/local/opt/openssl/lib
    $ export CPPFLAGS=-I/usr/local/opt/openssl/include
    $ export CXXFLAGS=-I/usr/local/opt/openssl/include
    $ ARGS="-DWITH_PROCPS=OFF -DOPENSSL_INCLUDE_DIR=/usr/local/opt/openssl/include/openssl -DCURVE=ALT_BN128"
    $ sed -i '' 's/STATIC/SHARED/' libff/CMakeLists.txt
    $ sed -i '' 's/STATIC/SHARED/' depends/CMakeLists.txt
    $ mkdir build
    $ cd build
    $ CXXFLAGS="-fPIC $CXXFLAGS" cmake $ARGS ..
    $ make && sudo make install

## Contact

You can find us in the DappHub chat at https://dapphub.chat/, especially the `#dev` channel.
