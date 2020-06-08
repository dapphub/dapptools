# hevm [![Build Status](https://travis-ci.com/dapphub/dapptools.svg?branch=master)](https://travis-ci.com/dapphub/dapptools)

The `hevm` project is an implementation of the Ethereum virtual machine (EVM) made specifically for symbolic execution, unit testing and debugging of smart contracts. It is developed by [DappHub](https://github.com/dapphub) and integrates especially well with the [`dapp` tool suite](https://github.com/dapphub/dapp). The `hevm` command line program can symbolically execute smart contracts, run unit tests, interactively debug contracts while showing the Solidity source, or run arbitrary EVM code. Computations can be performed using local state set up in a `dapp` testing harness, or fetched on demand from live networks using `rpc` calls.

### Usage

Note: some `hevm` commands (`dapp-test`, `interactive`) assumes the use of the `ds-test` framework for Solidity unit tests and the [`dapp` tool suite](https://github.com/dapphub/dapptools/tree/master/src/dapp), while others (`exec`, `symbolic`, ...) are available as standalone commands.

### Commands

    hevm -- Ethereum evaluator

    Usage: hevm [<options>] <command>
       or: hevm <command> --help

    Commands:

      symbolic        Execute symbolically, exploring all possible execution paths
      exec            Execute a given program with specified env & calldata
      equivalence     Prove equivalence between two programs using symbolic execution
      dapp-test       Run unit tests
      interactive     Browse and run unit tests interactively

      bc-test         Run an Ethereum Blockchain/GeneralState test
      vm-test         Run an Ethereum VMTest
      merkle-test     Run a merkle test file and ensure the root matches
      compliance      Run Blockchain or VMTest compliance report
  
      emacs           Emacs console
      version         Show hevm version
      flatten         Concat all dependencies for a given source file
      rlp             Decode a RLP encoded bytestring
      strip-metadata  Remove metadata from contract code bytestring

### Interactive debugger key bindings

  - `Esc`: exit debugger
  - `a`: step to start
  - `e`: step to end
  - `n`: step forwards by one instruction
  - `p`: step backwards by one instruction
  - `0`: choose the branch which does not jump
  - `1`: choose the branch which does jump
  - `N`: step to the next source position
  - `C-n`: step to the next source position and don't enter `CALL` or `CREATE`
  - `m`: toggle memory view
  - `h`: show key-binding help

### `hevm symbolic`

```sh
Usage: hevm symbolic [--code TEXT] [--calldata TEXT] [--func-sig TEXT]
                     [--address ADDR] [--caller ADDR] [--origin ADDR]
                     [--coinbase ADDR] [--value W256] [--nonce W256] [--gas W256]
                     [--number W256] [--timestamp W256] [--gaslimit W256]
                     [--gasprice W256] [--create] [--maxcodesize W256]
                     [--difficulty W256] [--debug] [--state STRING] [--rpc TEXT]
                     [--block W256] [--json-file STRING] [--get-models]
                     [--storage STORAGEMODE]
```

Run a symbolic execution against the given parameters, searching for assertion violations.

If an `assert` is reachable, a counterexample will be returned.

`--debug` enters an interactive debugger where the user can navigate the full execution space.

The default value for `calldata` and `caller` are symbolic values. If `--func-sig` is given,
calldata is assumed to be of the form suggested by the function signature.

If the `--get-models` flag is given, example input values will be returned for each possible execution path. 
This can be useful for automatic test case generation.

Storage can take one of three forms, defaulting to `Symbolic`:

- `Symbolic`: The default value of SLOAD is a symbolic value without further constraints. SLOAD and SSTORE can operate on symbolic locations.
- `Initial`: The default value of SLOAD is zero. SLOAD and SSTORE can operate on symbolic locations.
- `Concrete`: Storage defaults to zero or fetched from an rpc node if `--rpc` is provided. SLOAD or SSTOREs on symbolic locations will result in a runtime error.

Minimum required flags:

`--code` or (`--rpc` and `--address`).

### `hevm exec`

Run an EVM computation using specified parameters, using an interactive debugger when `--debug` flag is given.

```sh
Usage: hevm exec [--code TEXT] [--calldata TEXT] [--address ADDR] 
                 [--caller ADDR] [--origin ADDR] [--coinbase ADDR]
                 [--value W256] [--nonce W256] [--gas W256]
                 [--number W256] [--timestamp W256] [--gaslimit W256]
                 [--gasprice W256] [--create] [--maxcodesize W256]
                 [--difficulty W256] [--debug] [--state STRING] [--rpc TEXT]
                 [--block W256] [--json-file STRING]
```
Minimum required flags:

`--code` or (`--rpc` and `--address`).

If the execution returns an output, it will be written to stdout.

Exit code indicates whether the execution was successful or errored/reverted.

Simple example usage:
```sh
hevm exec --code 0x647175696e6550383480393834f3 --gas 0xff
```

Debug a mainnet transaction:
```sh
export ETH_RPC_URL=https://mainnet.infura.io/v3/YOUR_API_KEY_HERE
export TXHASH=0xd2235b9554e51e8ff5b3de62039d5ab6e591164b593d892e42b2ffe0e3e4e426
hevm exec --caller $(seth tx $TXHASH from) --address $(seth tx $TXHASH to) --calldata $(seth tx $TXHASH input) --rpc $ETH_RPC --block $(($(seth tx $TXHASH blockNumber)-1)) --gas $(seth tx $TXHASH gas) --debug
```

### `hevm equivalence`

```sh
Usage: hevm equiv --code-a TEXT --code-b TEXT [--func-sig TEXT]
```

Symbolically execute both the code given in `--code-a` and `--code-b` and try to prove equivalence between their output and storage.

If `--func-sig` is given, calldata is assumed to take the form of the function given.
If left out, calldata is a fully abstract buffer of at most 1024 bytes.

### `hevm dapp-test`

```
Usage: hevm dapp-test [--json-file STRING] [--dapp-root STRING] [--debug]
                      [--fuzz-runs INT] [--replay (TEXT,BYTESTRING)]
                      [--rpc TEXT] [--verbose INT] [--coverage] [--state STRING]
                      [--match STRING]
```

Run any ds-test testing functions. Run under the hood whenever `dapp test` or `dapp debug` is called. If testing functions have been given arguments, they will be randomly instantiated and run `--fuzz-runs` number of times. In `--debug` mode, property based tests will not be available unless given specific arguments using `--replay`.

### `hevm interactive`

Equivalent to `hevm dapp-test [options] --debug`

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
