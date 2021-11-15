# Dapp [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

`dapp` is a tool for building, testing and deploying smart contracts from the comfort of the command line.

As opposed to other tools, it does not use `rpc` to execute transactions. Instead,
it invokes the `hevm` cli directly. This is faster, and allows for a lot of flexibility
that isn't available in `rpc`, such as [fuzz testing](#property-based-testing), [symbolic execution](#symbolically-executed-tests), or [cheat codes to modify mainnet state](../hevm/README.md#cheat-codes).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

**Table of Contents**

- [Installing](#installing)
- [Basic usage: a tutorial](#basic-usage-a-tutorial)
  - [Building](#building)
  - [Unit testing](#unit-testing)
  - [Property based testing](#property-based-testing)
  - [Symbolically executed tests](#symbolically-executed-tests)
  - [Invariant testing](#invariant-testing)
  - [Testing against RPC state](#testing-against-rpc-state)
  - [Deployment](#deployment)
- [Configuration](#configuration)
  - [solc version](#solc-version)
- [Commands](#commands)
  - [`dapp init`](#dapp-init)
  - [`dapp build`](#dapp-build)
  - [`dapp test`](#dapp-test)
  - [`dapp debug`](#dapp-debug)
  - [`dapp create`](#dapp-create)
  - [`dapp address`](#dapp-address)
  - [`dapp install`](#dapp-install)
  - [`dapp uninstall`](#dapp-uninstall)
  - [`dapp update`](#dapp-update)
  - [`dapp upgrade`](#dapp-upgrade)
  - [`dapp testnet`](#dapp-testnet)
  - [`dapp verify-contract`](#dapp-verify-contract)
  - [`dapp mk-standard-json`](#dapp-mk-standard-json)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Installing

dapp is distributed as part of the [Dapp
tools](https://github.com/dapphub/dapptools) suite.

## Basic usage: a tutorial

Let's create a new `dapp` project. We make a new directory and initialize the `dapp` skeleton structure:

```sh
mkdir dapptutorial
dapp init
```

This creates two contracts, `Dapptutorial.sol` and `Dapptutorial.t.sol` in the `src` subdirectory and installs our testing library `ds-test` in the `lib` subdirectory.

`Dapptutorial.t.sol` is a testing contract with two trivial tests, which we can run with `dapp test`.

### Building

For the sake of this tutorial, let's change `Dapptutorial.sol` to a simple vault with an eth bounty that can be accessed by giving the password 42:

```solidity
pragma solidity ^0.8.6;

contract Dapptutorial {
    receive() external payable {
    }

    function withdraw(uint password) public {
        require(password == 42, "Access denied!");
        payable(msg.sender).transfer(address(this).balance);
    }
}
```

Compile the contract by running `dapp build`. If you didn't make any mistakes, you should simply see:

```
+ dapp clean
+ rm -rf out
```

### Unit testing

Let's write some tests for our vault. Change `Dapptutorial.t.sol` to the following. We'll go over whats going on in the next paragraph.

```solidity
contract DapptutorialTest is DSTest {
    Dapptutorial dapptutorial;

    function setUp() public {
        dapptutorial = new Dapptutorial();
    }

    function test_withdraw() public {
        payable(address(dapptutorial)).transfer(1 ether);
        uint preBalance = address(this).balance;
        dapptutorial.withdraw(42);
        uint postBalance = address(this).balance;
        assertEq(preBalance + 1 ether, postBalance);
    }

    function testFail_withdraw_wrong_pass() public {
        payable(address(dapptutorial)).transfer(1 ether);
        uint preBalance = address(this).balance;
        dapptutorial.withdraw(1);
        uint postBalance = address(this).balance;
        assertEq(preBalance + 1 ether, postBalance);
    }

    receive() external payable {
    }
}
```

In the `setUp()` function, we are deploying the `Dapptutorial` contract.
All following tests are run against the poststate of the `setUp()` function.
The `test_withdraw` function first deposits 1 eth and then withdraws it, by giving the correct password.
We check that the call was successful by comparing the pre and post balance of the testing account using
`assertEq`. You can try changing the right hand side to `postBalance + 1` and see what happens.
Finally, we are testing the case where the wrong password is given in `testFail_withdraw_wrong_pass`.
Any function prefixed with `testFail` is expected to fail, either with a `revert` or by violating an
assertion.
Finally, since a successful call to `withdraw` sends eth to the testing contract, we have to remember to
implement a `receive` function in it.

For more debugging information, run `dapp test` with the `-v` flag to print the calltrace for failing tests,
or enter the interactive debugger by running `dapp debug`.

### Property based testing

Now let's try something more interesting - property based testing and symbolically executed tests.

We can generailize our `test_withdraw` function to not use the hardcoded `1 ether`, but instead take
the value as a parameter:

```solidity
function test_withdraw(uint amount) public {
    payable(address(dapptutorial)).transfer(amount);
    uint preBalance = address(this).balance;
    dapptutorial.withdraw(42);
    uint postBalance = address(this).balance;
    assertEq(preBalance + amount, postBalance);
}
```

A test that takes at least one parameters is interpreted as a "property based test", or "fuzz test", and will
be run multiple times with different values given to the parameters. The number of times each test is run can be
configured by the `--fuzz-runs` flag and defaults to 100.

Running this test with `dapp test -v`, we see that this test actually fails with `error BalanceTooLow` for very high values of `amount`.

By default, the testing contract is given a balance of `2**96` wei, so we have to restrict the type of `amount` to `uint96` to make sure we don't try to transfer more than we have:

```solidity
function test_withdraw(uint96 amount) public {
    payable(address(dapptutorial)).transfer(amount);
    uint preBalance = address(this).balance;
    dapptutorial.withdraw(42);
    uint postBalance = address(this).balance;
    assertEq(preBalance + amount, postBalance);
}
```

If a counterexample is found, it can be replayed or analyzed in the debugger using the `--replay` flag.

### Symbolically executed tests

While property based testing runs each function repeatedly with new input values, symbolic execution leaves these
values symbolic and tries to explore each possible execution path. This gives a stronger guarantee and is more powerful than
property based testing, but is also more difficult, especially for complicated functions.

Continuing with our vault example, imagine that we forgot the password and did not have the source available.
We can symbolically explore all possibilities to find the one that lets us withdraw by writing a `proveFail` test:

```solidity
function proveFail_withdraw(uint guess) public {
    payable(address(dapptutorial)).transfer(1 ether);
    uint preBalance = address(this).balance;
    dapptutorial.withdraw(guess);
    uint postBalance = address(this).balance;
    assertEq(preBalance + 1 ether, postBalance);
}
```

When we run this with `dapp test`, we are given a counterexample:

```
Failure: proveFail_withdraw(uint256)

  Counterexample:

    result:   Successful execution
    calldata: proveFail_withdraw(42)
```

which demonstrates that if we give the password `42`, it is possible to withdraw from the vault.

The symbolic execution engine is backed by an SMT solver. When symbolically executing more complex tests you may encounter test failures with an `SMT Query Timeout` message. In this case, consider increasing the smt timeout using the `--smttimeout` flag or `DAPP_TEST_SMTTIMEOUT` environment variable (the default timeout is 60000 ms). Note that this timeout is per smt query not per test, and that each test may execute multiple queries (at least one query for each potential path through the test method).

For more reading on property based testing and symbolic execution, see [this tutorial on the Ethereum Foundation blog](https://fv.ethereum.org/2020/12/11/symbolic-execution-with-ds-test/).

### Invariant testing

While other forms of tests are always run against the post state of the `setUp()` function in the testing contract,
it can be also be useful to check whether a property is satisfied at every possible contract state. This can be done with
the `invariant*` testing type. When running an invariant test, hevm will invoke any state mutating function from all addresses returned
by a call to `targetContracts()`, if such a function exists in the testing contracts. If no such method exists, it will invoke methods from
any non-testing contract available after the `setUp()` function has been run, checking the `invariant*` after each run.

The `--depth` parameter determines how many transactions deep each test will run, while the `--fuzz-runs` parameter
determines how many times the whole process is repeated.

Note that a revert in any of the randomly generated call will not trigger a test failure. The goal of invariant tests is to find a state change that results in a violation of the assertions defined in the body of the test method, and since reverts do not result in a state change, they can be safely ignored. Reverts within the body of the `invariant*` test method will however still cause a test failure.

Example:

```solidity
function invariant_totalSupply() public {
    assertEq(token.totalSupply(), initialTotalSupply);
}
```

If a counterexample is found, it can be replayed or analyzed in the debugger using the `--replay` flag.

### SMTChecker testing

If you are using the standard JSON input mode and its field `settings.modelChecker.engine` is `all`, `bmc` or `chc`, [Solidity's SMTChecker](https://docs.soliditylang.org/en/latest/smtchecker.html) will be invoked when you run `dapp build`.
If you wish to use that mode, these steps are recommended:

- Run the usual compilation
- Generate a separate input JSON with the SMTChecker enabled: `export DAPP_SMTCHECKER=1 && dapp mk-standard-json &> dapp_smtchecker.json`
- Modify `settings.modelChecker` in the new JSON input accordingly. It is recommended that you use the [contracts field](https://docs.soliditylang.org/en/latest/smtchecker.html#verified-contracts) `settings.modelChecker.contracts` to specify the main contracts you want to verify.
- Tell `dapp` to use the new JSON as input: `export DAPP_STANDARD_JSON=./dapp_smtchecker.json`
- Run `dapp build`

You may also want to change the `settings.modelChecker.timeout` and/or other fields in different runs.

### Testing against RPC state

You can test how your contract interacts with already deployed contracts by
letting the testing state be fetched from rpc with the `--rpc` flag.

Running `dapp test` with the `--rpc` flag enabled will cause every state fetching operation
(such as SLOAD, EXTCODESIZE, CALL\*, etc.) to request the state from `$ETH_RPC_URL`.

For example, if you want to try out wrapping ETH you could define WETH in the `setUp()` function:

```solidity
import "ds-test/test.sol";

interface WETH {
    function balanceOf(address) external returns (uint);
    function deposit() external payable;
}

contract WethTest is DSTest {
    WETH weth;
    function setUp() public {
        weth = WETH(0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2);
    }

    function testWrap() public {
        assertEq(weth.balanceOf(address(this)), 0);
        weth.deposit{value :1 ether}();
        assertEq(weth.balanceOf(address(this)), 1 ether);
    }
}
```

With `ETH_RPC_URL` set, you can run `dapp test --rpc` on this test or `dapp debug --rpc` to step through
the `testWrap` function in the interactive debugger.

It is often useful to modify the state for testing purposes, for example to grant the testing contract
with a balance of a particular token. This can be done using [`hevm cheat codes`](../hevm/README.md#cheat-codes).

### Deployment

To deploy a contract, you can use `dapp create`:

```solidity
dapp create Dapptutorial [<constructorArgs>] [<options>]
```

The `--verify` flag verifies the contract on etherscan (requires `ETHERSCAN_API_KEY`).

## Configuration

The commands of `dapp` can be customized with environment variables or flags.
These variables can be set at the prompt or in a `.dapprc` file.

Below is a list of the environment variables recognized by `dapp`. You can additionally control
various block parameters when running unit tests by using the [hevm specific environment
variables](../hevm/README.md#environment-variables).

| Variable                   | Default                    | Synopsis                                                                                                                                           |
| -------------------------- | -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| `DAPP_SRC`                 | `src`                      | Directory for the project's Solidity contracts                                                                                                     |
| `DAPP_LIB`                 | `lib`                      | Directory for installed Dapp packages                                                                                                              |
| `DAPP_OUT`                 | `out`                      | Directory for compilation artifacts                                                                                                                |
| `DAPP_ROOT`                | `.`                        | Root directory of compilation                                                                                                                      |
| `DAPP_SOLC_VERSION`        | `0.8.6`                    | Solidity compiler version to use                                                                                                                   |
| `DAPP_SOLC`                | n/a                        | solc binary to use                                                                                                                                 |
| `DAPP_LIBRARIES`           | automatically deployed     | Library addresses to link to                                                                                                                       |
| `DAPP_SKIP_BUILD`          | n/a                        | Avoid compiling this time                                                                                                                          |
| `DAPP_COVERAGE`            | n/a                        | Print coverage data                                                                                                                                |
| `DAPP_LINK_TEST_LIBRARIES` | `1` when testing; else `0` | Compile with libraries                                                                                                                             |
| `DAPP_VERIFY_CONTRACT`     | `yes`                      | Attempt Etherscan verification                                                                                                                     |
| `DAPP_ASYNC`               | n/a                        | Set to `yes` to skip waiting for etherscan verification to succeed                                                                                 |
| `DAPP_STANDARD_JSON`       | `$(dapp mk-standard-json)` | [Solidity compilation options](https://docs.soliditylang.org/en/latest/using-the-compiler.html#compiler-input-and-output-json-description)         |
| `DAPP_SMTCHECKER`          | n/a                        | Set to `1` to output the default model checker settings when using `dapp mk-standard-json`. Running `dapp build` will invoke the SMTChecker.       |
| `DAPP_REMAPPINGS`          | `$(dapp remappings)`       | [Solidity remappings](https://docs.soliditylang.org/en/latest/using-the-compiler.html#path-remapping)                                              |
| `DAPP_BUILD_OPTIMIZE`      | `0`                        | Activate Solidity optimizer (`0` or `1`)                                                                                                           |
| `DAPP_BUILD_OPTIMIZE_RUNS` | `200`                      | Set the optimizer runs                                                                                                                             |
| `DAPP_TEST_MATCH`          | n/a                        | Only run test methods matching a regex                                                                                                             |
| `DAPP_TEST_VERBOSITY`      | `0`                        | Sets how much detail `dapp test` logs. Verbosity `1` shows traces for failing tests, `2` shows logs for all tests, `3` shows traces for all tests  |
| `DAPP_TEST_FFI `           | `0`                        | Allow use of the ffi cheatcode in tests (`0` or `1`)                                                                                               |
| `DAPP_TEST_FUZZ_RUNS`      | `200`                      | How many iterations to use for each property test in your project                                                                                  |
| `DAPP_TEST_DEPTH`          | `20`                       | Number of transactions to sequence per invariant cycle                                                                                             |
| `DAPP_TEST_SMTTIMEOUT`     | `60000`                    | Timeout passed to the smt solver for symbolic tests (in ms, and per smt query)                                                                     |
| `DAPP_TEST_MAX_ITERATIONS` | n/a                        | The number of times hevm will revisit a particular branching point when symbolically executing                                                     |
| `DAPP_TEST_SOLVER`         | `z3`                       | Solver to use for symbolic execution (`cvc4` or `z3`)                                                                                              |
| `DAPP_TEST_MATCH`          | n/a                        | Regex used to determine test methods to run                                                                                                        |
| `DAPP_TEST_COV_MATCH`      | n/a                        | Regex used to determine which files to print coverage reports for. Prints all imported files by default (excluding tests and libs).                |
| `DAPP_TEST_REPLAY`         | n/a                        | Calldata for a specific property test case to replay in the debugger                                                                               |
| `HEVM_RPC`                 | n/a                        | Set to `yes` to have `hevm` fetch state from rpc when running unit tests                                                                           |
| `ETH_RPC_URL`              | n/a                        | The url of the rpc server that should be used for any rpc calls                                                                                    |
| `DAPP_TESTNET_RPC_PORT`    | `8545`                     | Which port to expose the rpc server on when running `dapp testnet`                                                                                 |
| `DAPP_TESTNET_RPC_ADDRESS` | `127.0.0.1`                | Which ip address to bind the rpc server to when running `dapp testnet`                                                                             |
| `DAPP_TESTNET_CHAINID`     | `99`                       | Which chain id to use when running `dapp testnet`                                                                                                  |
| `DAPP_TESTNET_PERIOD`      | `0`                        | Blocktime to use for `dapp testnet`. `0` means blocks are produced instantly as soon as a transaction is received                                  |
| `DAPP_TESTNET_ACCOUNTS`    | `0`                        | How many extra accounts to create when running `dapp testnet` (At least one is always created)                                                     |
| `DAPP_TESTNET_gethdir`     | `$HOME/.dapp/testnet`      | Root directory that should be used for `dapp testnet` data                                                                                         |
| `DAPP_TESTNET_SAVE`        | n/a                        | Name of the subdirectory under `${DAPP_TESTNET_gethdir}/snapshots` where the chain data from the current `dapp testnet` invocation should be saved |
| `DAPP_TESTNET_LOAD`        | n/a                        | Name of the subdirectory under `${DAPP_TESTNET_gethdir}/snapshots` from which `dapp testnet` chain data should be loaded                           |
| `DAPP_BUILD_EXTRACT`       | n/a                        | Set to a non null value to output `.abi`, `.bin` and `.bin-runtime` when using `dapp build`. Uses legacy build mode                                |
| `DAPP_BUILD_LEGACY`        | n/a                        | Set to a non null value to compile using the `--combined-json` flag. This is provided for compatibility with older workflows                       |

A global (always loaded) config file is located in `~/.dapprc`. A local `.dapprc` can also be defined in your project's root, which overrides variables in the global config.

Whenever you run a `dapp` command the `.dapprc` files are sourced in order (global first, then the one in the current working directory, if it exists). If you wish to set configuration variables, you must use `export` as below:

```sh
export DAPP_SOLC_VERSION=0.8.6
export DAPP_REMAPPINGS=$(cat remappings.txt)
export DAPP_BUILD_OPTIMIZE=1
export DAPP_BUILD_OPTIMIZE_RUNS=1000000000
export DAPP_TEST_VERBOSITY=1
```

Under the hood `.dapprc` is interpreted as a shell script, which means you can add additional scripting logic which will be run whenever you use `dapp`. For example if you wanted to fuzz for many iterations in CI and only a few locally you could add this to your `.dapprc`:

```sh
if [ "$CI" == "true" ]
then
  export DAPP_TEST_FUZZ_RUNS=1000000 # In CI we want to fuzz for a long time.
else
  export DAPP_TEST_FUZZ_RUNS=1000 # When developing locally we only want to fuzz briefly.
fi
```

### Precedence

There are multiple places to specify configuration options. They are read with the following precedence:

1. command line flags
2. local `.dapprc`
3. global `.dapprc`
4. locally set environment variables

### solc version

You can specify a custom `solc` version to run within `dapp` with `dapp --use <arg>`.
If the argument is of the form `solc:x.y.z`, the appropriate solc version will temporarily installed.
If the argument contains a `/`, it is interpreted as a path to a solc binary to be used.

You may also specify a solc version using the `DAPP_SOLC_VERSION` environment variable, which is equivalent to running `dapp --use solc:${DAPP_SOLC_VERSION}` manually.

You can install any supported `solc` "standalone" (i.e. add it to your `$PATH`) with:

```sh
nix-env -iA solc-static-versions.solc_x_y_z \
  -if https://github.com/dapphub/dapptools/tarball/master
```

For a list of the supported `solc` versions, check [`solc-static-versions.nix`](/nix/solc-static-versions.nix).

## Commands

### `dapp init`

    dapp-init -- bootstrap a new dapp
    Usage: dapp init

Initializes the current directory to the default `dapp` structure,
installing `ds-test` and creating two boilerplate contracts in the `src` directory.

### `dapp build`

    dapp-build -- compile the source code
    Usage: dapp build [--extract]

    --extract:  After building, write the .abi, .bin and .bin-runtime. Implies `--legacy`
        files from the solc json into $DAPP_OUT. Beware of contract
        name collisions. This is provided for compatibility with older
        workflows.
    --optimize: activate the solidity optimizer.
    --legacy:   Compile using the `--combined-json` flag. Some options are
        missing from this format. This is provided for compatibility with older
        workflows.

Compiles the contracts in the `src` directory.
The compiler options of the build are generated by the [`dapp mk-standard-json`](#dapp-mk-standard-json) command,
which infers most options from the project structure. For more customizability, you can define your own configuration json
by setting the file to the environment variable `DAPP_STANDARD_JSON`.

By default, `dapp build` uses [`dapp remappings`](#dapp-remappings) to resolve Solidity import paths.

You can override this with the `DAPP_REMAPPINGS` environment variable.

### `dapp test`

    Usage: dapp test [<options>]

    Options:
        -v, --verbose             trace output for failing tests
        --coverage                print coverage data
        --verbosity <number>      sets the verbosity to <number>
        --depth=<number>          number of transactions to sequence per invariant cycle
        --fuzz-runs <number>      number of times to run fuzzing tests
        --replay <string>         rerun a particular test case
        -m, --match <string>      only run test methods matching regex
        --cov-match <string>      only print coverage for files matching regex

    RPC options:
        --rpc                     fetch remote state via ETH_RPC_URL
        --rpc-url <url>           fetch remote state via <url>
        --rpc-block <number>      block number (latest if not specified)

    SMT options:
        --smttimeout <number>     timeout passed to the smt solver in ms (default 60000)
        --solver <string>         name of the smt solver to use (either "z3" or "cvc4")
        --max-iterations <number> number of times we may revisit a particular branching point during symbolic execution

dapp tests are written in Solidity using the `ds-test` module. To install it, run

```sh
dapp install ds-test
```

Every contract which inherits from `DSTest` will be treated as a test contract, if it has a `setUp()` function, it will be run before every test.

Every function prefixed with `test` is expected to succeed, while functions
prefixed by `testFail` are expected to fail.

Functions prefixed with `prove` are run symbolically, expecting success while functions
prefixed `proveFail` are run symbolically expecting failure.

The `-v` flag prints call traces for failing tests, `--verbosity 2` will show `ds-test` events for
all tests, while `--verbosity 3` will show call traces for all tests.

If you provide `--rpc`, state will be fetched via rpc. Local changes take priority.

You can configure the testing environment using [hevm specific environment variables](https://github.com/dapphub/dapptools/tree/master/src/hevm#environment-variables).

To modify local state even more, you can use [hevm cheat codes](https://github.com/dapphub/dapptools/tree/master/src/hevm#cheat-codes).

If your test function takes arguments, they will be randomly instantiated and the function will be run multiple times.

The number of times run is configurable using `--fuzz-runs`.

To step through a test in `hevm` interactive debugger, use [`dapp debug`](#dapp-debug).

`dapp test --match <regex>` will only run tests that match the given
regular expression. This will be matched against the file path of the
test file, followed by the contract name and the test method, in the
form `src/my_test_file.sol:TestContract.test_name()`. For example, to
only run tests from the contract `ContractA`:

```
dapp test --match ':ContractA\.'
```

To run all tests, from all contracts, that contain either `foo` or `bar`
in the test name:

```
dapp test --match '(foo|bar)'
```

To only run tests called 'test_this()' from `TheContract` in the
`src/test/a.t.sol` file:

```
dapp test --match 'src/test/a\.t\.sol:TheContract\.test_this\(\)'
```

By default, `dapp test` also recompiles your contracts.
To skip this, you can set the environment variable `DAPP_SKIP_BUILD=1`.

If you have any libraries in `DAPP_SRC` or `DAPP_LIB` with nonzero bytecode,
they will be deployed locally and linked to by any contracts referring to them.
This can be skipped by setting `DAPP_LINK_TEST_LIBRARIES=0`.

### `dapp debug`

    dapp-debug -- run unit tests interactively (hevm)
    Usage: dapp debug [<options>]

    Options:
       --rpc                 fetch remote state via ETH_RPC_URL
       --rpc-url=<url>       fetch remote state via <url>
       --rpc-block=<number>  block number (latest if not specified)

Enters the interactive debugger. See the [hevm README](../hevm/README.md#interactive-debugger-key-bindings)
for key bindings for navigation.

### `dapp create`

    dapp-create -- deploy a compiled contract (--verify on Etherscan)
    Usage: dapp create <contractname> or
        dapp create <path>:<contractname>
    Add --verify and export your ETHERSCAN_API_KEY to auto-verify on Etherscan

### `dapp address`

    dapp-address -- determine address of newly generated contract
    Usage: dapp address <sender> <nonce>

### `dapp install`

    dapp-install -- install a smart contract library
    Usage: dapp install <lib>
    <lib> may be:
    - a Dapphub repo (ds-foo)
    - the URL of a Dapphub repo (https://github.com/dapphub/ds-foo)
    - a path to a repo in another Github org (org-name/repo-name)

You can also specify a version (or branch / commit hash) for the repository by
suffixing the URL with `@<version>`. `dapp install` will then proceed to
clone the repository and then `git checkout --recurse-submodules $version`.

If the project you want to install does not follow the typical `dapp` project structure,
you may need to configure the `DAPP_REMAPPINGS` environment variable to be able to find
it. For an example, see [this repo](https://github.com/dapp-org/radicle-contracts-tests/).


### `dapp uninstall`

    dapp-uninstall -- remove a smart contract library
    Usage: dapp uninstall <lib>

### `dapp update`

    dapp-update -- fetch all upstream lib changes
    Usage: dapp update [<lib>]

Updates a project submodule in the `lib` subdirectory.

### `dapp snapshot`

    dapp-snapshot -- creates a snapshot of each test's gas usage
    Usage: dapp snapshot

Saves a snapshot of each concrete test's gas usage in a `.gas-snapshot` file.

### `dapp check-snapshot`

    dapp-check-snapshot -- check snapshot is up to date
    Usage: dapp check-snapshot

Runs `dapp snapshot` and exits with an error code if its output does not match the current `.gas-snapshot` file.

### `dapp upgrade`

    dapp-upgrade -- pull & commit all upstream lib changes
    Usage: dapp upgrade [<lib>]

### `dapp testnet`

Spins up a geth testnet.

### `dapp verify-contract`

    dapp-verify-contract -- verify contract source on etherscan
    Usage: dapp verify-contract <path>:<contractname> <address> [constructorArgs]

Example: `dapp verify-contract src/auth/authorities/RolesAuthority.sol:RolesAuthority 0x9ed0e..`

Requires `ETHERSCAN_API_KEY` to be set.

`seth chain` will be used to determine on which network the contract is to be verified.

Automatically run when the `--verify` flag is passed to `dapp create`.

### `dapp mk-standard-json`

Generates a Solidity settings input json using the structure of the current directory.

The following environment variables can be used to override settings:

- `DAPP_SRC`
- `DAPP_REMAPPINGS`
- `DAPP_BUILD_OPTIMIZE`
- `DAPP_BUILD_OPTIMIZE_RUNS`
- `DAPP_LIBRARIES`
- `DAPP_SMTCHECKER`
