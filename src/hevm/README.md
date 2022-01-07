# hevm [![Build Status](https://travis-ci.com/dapphub/dapptools.svg?branch=master)](https://travis-ci.com/dapphub/dapptools)

The `hevm` project is an implementation of the Ethereum virtual machine (EVM) made specifically for symbolic execution, unit testing and debugging of smart contracts. It is developed by [DappHub](https://github.com/dapphub) and integrates especially well with the [`dapp` tool suite](https://github.com/dapphub/dapp). The `hevm` command line program can symbolically execute smart contracts, run unit tests, interactively debug contracts while showing the Solidity source, or run arbitrary EVM code. Computations can be performed using local state set up in a `dapp` testing harness, or fetched on demand from live networks using `rpc` calls.

### Usage

Note: some `hevm` commands (`dapp-test`) assume the use of the `ds-test` framework for Solidity unit tests and the [`dapp` tool suite](https://github.com/dapphub/dapptools/tree/master/src/dapp), while others (`exec`, `symbolic`, ...) are available as standalone commands.

### Commands

    hevm -- Ethereum evaluator

    Usage: hevm [<options>] <command>
       or: hevm <command> --help

    Commands:

      symbolic        Execute symbolically, exploring all possible execution paths
      exec            Execute a given program with specified env & calldata
      equivalence     Prove equivalence between two programs using symbolic execution
      dapp-test       Run unit tests

      bc-test         Run an Ethereum Blockchain/GeneralState test
      merkle-test     Run a merkle test file and ensure the root matches
      compliance      Run Blockchain compliance report

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
- `P`: step previous source position
- `C-n`: step to the next source position and don't enter `CALL` or `CREATE`
- `C-p`: step previous source position without entering
- `m`: toggle memory view
- `Down` : step to next entry in the callstack / Scroll memory pane\n" <>
- `Up` : step to previous entry in the callstack / Scroll memory pane\n" <>
- `h`: show key-binding help

### `hevm symbolic`

```sh
Usage: hevm symbolic [--code TEXT] [--calldata TEXT] [--address ADDR]
                     [--caller ADDR] [--origin ADDR] [--coinbase ADDR]
                     [--value W256] [--nonce W256] [--gas W256] [--number W256]
                     [--timestamp W256] [--gaslimit W256] [--gasprice W256]
                     [--create] [--maxcodesize W256] [--difficulty W256]
                     [--chainid W256] [--rpc TEXT] [--block W256]
                     [--state STRING] [--cache STRING] [--json-file STRING]
                     [--dapp-root STRING] [--storage-model STORAGEMODEL]
                     [--sig TEXT] [--arg STRING]... [--debug] [--get-models]
                     [--show-tree] [--smttimeout INTEGER]
                     [--max-iterations INTEGER] [--solver TEXT] [--smtdebug]
                     [--assertions [WORD256]] [--ask-smt-iterations INTEGER]

Available options:
  -h,--help                Show this help text
  --code TEXT              Program bytecode
  --calldata TEXT          Tx: calldata
  --address ADDR           Tx: address
  --caller ADDR            Tx: caller
  --origin ADDR            Tx: origin
  --coinbase ADDR          Block: coinbase
  --value W256             Tx: Eth amount
  --nonce W256             Nonce of origin
  --gas W256               Tx: gas amount
  --number W256            Block: number
  --timestamp W256         Block: timestamp
  --gaslimit W256          Tx: gas limit
  --gasprice W256          Tx: gas price
  --create                 Tx: creation
  --maxcodesize W256       Block: max code size
  --difficulty W256        Block: difficulty
  --chainid W256           Env: chainId
  --rpc TEXT               Fetch state from a remote node
  --block W256             Block state is be fetched from
  --state STRING           Path to state repository
  --cache STRING           Path to rpc cache repository
  --json-file STRING       Filename or path to dapp build output (default:
                           out/*.solc.json)
  --dapp-root STRING       Path to dapp project root directory (default: . )
  --storage-model STORAGEMODEL
                           Select storage model: ConcreteS, SymbolicS (default)
                           or InitialS
  --sig TEXT               Signature of types to decode / encode
  --arg STRING             Values to encode
  --debug                  Run interactively
  --get-models             Print example testcase for each execution path
  --show-tree              Print branches explored in tree view
  --smttimeout INTEGER     Timeout given to SMT solver in milliseconds (default:
                           60000)
  --max-iterations INTEGER Number of times we may revisit a particular branching
                           point
  --solver TEXT            Used SMT solver: z3 (default) or cvc4
  --smtdebug               Print smt queries sent to the solver
  --assertions [WORD256]   Comma seperated list of solc panic codes to check for
                           (default: everything except arithmetic overflow)
  --ask-smt-iterations INTEGER
                           Number of times we may revisit a particular branching
                           point before we consult the smt solver to check
                           reachability (default: 5)
```

Run a symbolic execution against the given parameters, searching for assertion violations.

Counterexamples will be returned for any reachable assertion violations. Where an assertion
violation is defined as either an execution of the invalid opcode (`0xfe`), or a revert with a
message of the form `abi.encodeWithSelector('Panic(uint256)', errCode)` with `errCode` being one of
the predefined solc assertion codes defined
[here](https://docs.soliditylang.org/en/latest/control-structures.html#panic-via-assert-and-error-via-require).

By default hevm ignores assertion violations that result from arithmetic overflow (`Panic(0x11)`),
although this behaviour can be customised via the `--assertions` flag. For example, the following
will return counterexmaples for arithmetic overflow (`0x11`) and user defined assertions (`0x01`):

```
hevm symbolic --code $CODE --assertions '[0x01, 0x11]'
```

`--debug` enters an interactive debugger where the user can navigate the full execution space.

The default value for `calldata` and `caller` are symbolic values, but can be specialized to concrete functions with their corresponding flags.

One can also specialize specific arguments to a function signature, while leaving others abstract.
If `--sig` is given, calldata is assumed to be of the form suggested by the function signature. With this flag, specific arguments can be instantiated to concrete values via the `--arg` flag.

This is best illustrated through a few examples:

Calldata specialized to the bytestring `0xa9059cbb` followed by 64 symbolic bytes:

```sh
hevm symbolic --sig "transfer(address,uint256)" --code $(<dstoken.bin-runtime)
```

Calldata specialized to the bytestring `0xa9059cbb0000000000000000000000007cfa93148b0b13d88c1dce8880bd4e175fb0dedf` followed by 32 symbolic bytes.

```sh
hevm symbolic --sig "transfer(address,uint256)" --arg 0x7cFA93148B0B13d88c1DcE8880bd4e175fb0DeDF --code $(<dstoken.bin-runtime)
```

Calldata specialized to the bytestring `0xa9059cbb` followed by 32 symbolic bytes, followed by the bytestring `0000000000000000000000000000000000000000000000000000000000000000`:

```sh
hevm symbolic --sig "transfer(address,uint256)" --arg "<symbolic>" --arg 0 --code $(<dstoken.bin-runtime)
```

If the `--get-models` flag is given, example input values will be returned for each possible execution path.
This can be useful for automatic test case generation.

The default timeout for SMT queries is no timeout. If your program is taking longer than a couple of minutes to run,
you can experiment with configuring the timeout to somewhere around 10s by doing `--smttimeout 10000`

Storage can take one of three forms, defaulting to `SymbolicS` unless `--create` is provided, in which case it defaults to `InitialS`:

- `SymbolicS`: The default value of SLOAD is a symbolic value without further constraints. SLOAD and SSTORE can operate on symbolic locations.
- `InitialS`: The default value of SLOAD is zero. SLOAD and SSTORE can operate on symbolic locations.
- `ConcreteS`: Storage defaults to zero or fetched from an rpc node if `--rpc` is provided. SLOAD or SSTOREs on symbolic locations will result in a runtime error.

`hevm` uses an eager approach for symbolic execution, meaning that it will first attempt to explore
all branches in the program (without querying the smt solver to check if they are reachable or not).
Once the full execution tree has been explored, the postcondition is checked against all leaves, and
the solver is invoked to check reachability for branches where a postcondition violation could
occur. While our tests have shown this approach to be significantly faster, when applied without
limits it would always result in infitie exploration of code involving loops, so after some
predefined number of iterations (controlled by the `--ask-smt-iterations` flag), the solver will be
invoked to check whether a given loop branch is reachable. In cases where the number of loop
iterations is known in advance, you may be able to speed up execution by setting this flag to an
appropriate value.

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

Debug a mainnet transaction (older transactions require archive node):

```sh
export ETH_RPC_URL=https://mainnet.infura.io/v3/YOUR_API_KEY_HERE
export TXHASH=0xd2235b9554e51e8ff5b3de62039d5ab6e591164b593d892e42b2ffe0e3e4e426
hevm exec --caller $(seth tx $TXHASH from) --address $(seth tx $TXHASH to) --calldata $(seth tx $TXHASH input) --rpc $ETH_RPC_URL --block $(($(seth tx $TXHASH blockNumber)-1)) --gas $(seth tx $TXHASH gas) --debug
```

If `--state` is provided, hevm will load and save state to the directory provided. Git is used as a storage model, with each hevm run creating a new commit. The directory provided must be a non empty git repository. After a run you can see the state diff by simply looking at the commit (for example `git show HEAD`).

```sh
mkdir mystate
cd mystate && git init && git commit --allow-empty -m "init" && cd ..
hevm exec --code 0x600160015500 --state mystate --gas 0xffffff
cd mystate && git show HEAD
```

### `hevm equivalence`

```sh
Usage: hevm equivalence --code-a TEXT --code-b TEXT [--sig TEXT]
```

Symbolically execute both the code given in `--code-a` and `--code-b` and try to prove equivalence between their outputs and storages.

If `--sig` is given, calldata is assumed to take the form of the function given.
If left out, calldata is a fully abstract buffer of at most 256 bytes.

### `hevm dapp-test`

```
Usage: hevm dapp-test [--json-file STRING] [--dapp-root STRING] [--debug]
                      [--fuzz-runs INT] [--replay (TEXT,BYTESTRING)] [--depth INT]
                      [--rpc TEXT] [--verbose INT] [--coverage] [--state STRING]
                      [--match STRING] [--smttimeout INT] [--max-iterations INT]
                      [--solver STRING] [--cache STRING] [--cov-match STRING]
```

Run any ds-test testing functions. Run under the hood whenever `dapp test` or `dapp debug` is called. Testing functions prefixed with `test` will be executed concretely. If concrete test functions have been given arguments, they will be randomly instantiated and run `--fuzz-runs` number of times. If testing functions are prefixed with `prove` they will be symbolically executed. In `--debug` mode, property based tests will not be available unless given specific arguments using `--replay`.

The `smttimeout`, `max-iterations` and `solver` options have the same semantics as in `hevm symbolic`

### Environment Variables

These environment variables can be used to control block parameters:

| Variable               | Default                                      | Synopsis                                                                                                      |
| ---------------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| `DAPP_TEST_ADDRESS`    | `0xb4c79daB8f259C7Aee6E5b2Aa729821864227e84` | The address to deploy the test contract to                                                                    |
| `DAPP_TEST_CALLER`     | `0x00a329c0648769a73afac7f9381e08fb43dbea72` | The address to set `msg.sender` to when calling into the test contract                                        |
| `DAPP_TEST_ORIGIN`     | `0x00a329c0648769a73afac7f9381e08fb43dbea72` | The address to set `tx.orgin` to when calling into the test contract                                          |
| `DAPP_TEST_GAS_CREATE` | `0xffffffffffff`                             | The gas to provide when creating the testing contract                                                         |
| `DAPP_TEST_GAS_CALL`   | `0xffffffffffff`                             | The gas to provide to each call made to the testing contract                                                  |
| `DAPP_TEST_BALANCE`    | `0xffffffffffffffffffffffff`                 | The balance to provide to `DAPP_TEST_ADDRESS`                                                                 |
| `DAPP_TEST_COINBASE`   | `0x0000000000000000000000000000000000000000` | The coinbase address. Will be set to the coinbase for the block at `DAPP_TEST_NUMBER` if rpc is enabled       |
| `DAPP_TEST_NUMBER`     | `0`                                          | The block number. Will be set to the latest block if rpc is enabled                                           |
| `DAPP_TEST_TIMESTAMP`  | `0`                                          | The block timestamp. Will be set to the timestamp for the block at `DAPP_TEST_NUMBER` if rpc is enabled       |
| `DAPP_TEST_GAS_LIMIT`  | `0`                                          | The block gas limit to use                                                                                    |
| `DAPP_TEST_GAS_PRICE`  | `0`                                          | The gas price to use                                                                                          |
| `DAPP_TEST_DIFFICULTY` | `0`                                          | The block difficulty. Will be set to the difficulty for the the block at `DAPP_TEST_NUMBER` if rpc is enabled |

## Installing

### Nix

`hevm` is distributed as part of the [Dapp tools](https://github.com/dapphub/dapptools) suite.

### Building with Stack or Cabal

If you can't or won't use Nix, the easiest way especially if you don't have GHC (the Haskell compiler) installed already, is to use [Stack](https://docs.haskellstack.org/en/stable/README/), which can take care of installing GHC for you. These commands should work:

    $ curl -sSL https://get.haskellstack.org/ | sh
    $ git clone https://github.com/dapphub/dapptools.git
    $ cd dapptools/src/hevm && stack setup && stack install

Also, hevm is in Hackage so you can execute `stack install hevm` to get it up and running.

If you prefer to use your own installation of GHC and the basic Haskell package manager, Cabal, simply run:

    $ git clone https://github.com/dapphub/dapptools.git
    $ cd dapptools/src/hevm && cabal v2-update && cabal v2-configure && cabal v2-install

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

### Cheat codes

Since Hevm is an EVM implementation mainly dedicated to testing and exploration, it features a set of `cheat codes` which can manipulate the environment in which the execution is run.

These can be accessed by calling into a contract (typically called `Hevm`) at address `0x7109709ECfa91a80626fF3989D68f67F5b1DD12D`, which implements the following methods:

- `function warp(uint x) public`
  Sets the block timestamp to `x`.

- `function roll(uint x) public`
  Sets the block number to `x`.

- `function store(address c, bytes32 loc, bytes32 val) public`
  Sets the slot `loc` of contract `c` to `val`.

- `function load(address c, bytes32 loc) public returns (bytes32 val)`
  Reads the slot `loc` of contract `c`.

- `function sign(uint sk, bytes32 digest) public returns (uint8 v, bytes32 r, bytes32 s)`
  Signs the `digest` using the private key `sk`. Note that signatures produced via `hevm.sign` will leak the private key.

- `function addr(uint sk) public returns (address addr)`
  Derives an ethereum address from the private key `sk`. Note that `hevm.addr(0)` will fail with
  `BadCheatCode` as `0` is an invalid ECDSA private key.

- `function ffi(string[] calldata) external returns (bytes memory)`
  Executes the arguments as a command in the system shell and returns stdout. Expects abi encoded values to be returned from the shell or an error will be thrown. Note that this
  cheatcode means test authors can execute arbitrary code on user machines as part of a call to `dapp test`, for this reason all calls to `ffi` will fail unless the `--ffi` flag is passed.

## Contact

You can find us in the DappHub chat at https://dapphub.chat/, especially the `#dev` channel.
