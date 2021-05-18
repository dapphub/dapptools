# Dapp [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

`dapp` is a tool for building, testing and deploying smart contracts from the comfort of the command line.

As opposed to other tools, it does not use `rpc` to execute transactions. Instead,
it invokes the `hevm` cli directly. This is faster, and allows for a lot of flexibility
that isn't available in `rpc`, such as [fuzz testing](#dapp-test-flags), symbolic execution, or [cheat codes to modify mainnet state](../hevm/README.md#cheat-codes).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Installing](#installing)
- [Basic usage: a tutorial](#basic-usage-a-tutorial)
  - [Building](#building)
  - [Unit testing](#unit-testing)
  - [Property based testing](#property-based-testing)
  - [Symbolically executed tests](#symbolically-executed-tests)
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
  - [`dapp update`](#dapp-update)
  - [`dapp upgrade`](#dapp-upgrade)
  - [`dapp testnet`](#dapp-testnet)
  - [`dapp verify-contract`](#dapp-verify-contract)
  - [`dapp mk-standard-json`](#dapp-mk-standard-json)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Installing
------------------------------------------------------------------------

dapp is distributed as part of the [Dapp
tools](https://github.com/dapphub/dapptools) suite.

## Basic usage: a tutorial

Lets create a new `dapp` project. We make a new directory and initialize the `dapp` skeleton structure:
```sh
mkdir dapptutorial
dapp init
```
This creates two contracts, `Dapptutorial.sol` and ``Dapptutorial.t.sol` in the `src` subdirectory and installs our testing library `ds-test` in the `lib` subdirectory.

`Dapptutorial.t.sol` is a testing contract with two trivial tests, which we can run with `dapp test`.

### Building

For the sake of this tutorial, lets change `Dapptutorial.sol` to a simple vault with an eth bounty that can be accessed by giving the password 42:

```solidity
pragma solidity ^0.6.7;

contract Dapptutorial {
    receive() external payable {
    }

    function withdraw(uint password) public {
        require(password == 42, "Access denied!");
        msg.sender.transfer(address(this).balance);
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
        address(dapptutorial).transfer(1 ether);
        uint preBalance = address(this).balance;
        dapptutorial.withdraw(42);
        uint postBalance = address(this).balance;
        assertEq(preBalance + 1 ether, postBalance);
    }

    function testFail_withdraw_wrong_pass() public {
        address(dapptutorial).transfer(1 ether);
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

Now lets try something more interesting - property based testing and symbolically executed tests.

We can generailize our `test_withdraw` function to not use the hardcoded `1 ether`, but instead take
the value as a parameter:
```solidity
function test_withdraw(uint amount) public {
    address(dapptutorial).transfer(amount);
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
    address(dapptutorial).transfer(amount);
    uint preBalance = address(this).balance;
    dapptutorial.withdraw(42);
    uint postBalance = address(this).balance;
    assertEq(preBalance + amount, postBalance);
}
```

### Symbolically executed tests

While property based testing runs each function repeatedly with new input values, symbolic execution leaves these 
values symbolic and tries to explore each possible execution path. This gives a stronger guarantee and is more powerful than
property based testing, but is also more difficult, especially for complicated functions.

Continuing with our vault example, imagine that we forgot the password and did not have the source available. 
We can symbolically explore all possibilities to find the one that lets us withdraw by writing a `proveFail` test:

```solidity
function proveFail_withdraw(uint guess) public {
    address(dapptutorial).transfer(1 ether);
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

For more reading on property based testing and symbolic execution, see [this tutorial on the Ethereum Foundation blog](https://fv.ethereum.org/2020/12/11/symbolic-execution-with-ds-test/).

### Testing against RPC state

You can test how your contract interacts with already deployed contracts by 
letting the testing state be fetched from rpc with the `--rpc` flag.

Running `dapp test` with the `--rpc` flag enabled will cause every state fetching operation
(such as SLOAD, EXTCODESIZE, CALL*, etc.) to request the state from `$ETH_RPC_URL`.

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

| Variable                   | Default                    | Synopsis                                                                                                                                   |
|----------------------------|----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| `DAPP_SRC`                 | `src`                      | Project Solidity source directory                                                                                                          |
| `DAPP_LIB`                 | `lib`                      | Directory for installed Dapp packages                                                                                                      |
| `DAPP_OUT`                 | `out`                      | Directory for compilation artifacts                                                                                                        |
| `DAPP_ROOT`                | `.`                        | Root directory of compilation                                                                                                              |
| `DAPP_SOLC_VERSION`        | n/a                        | Solidity compiler version to use                                                                                                           |
| `DAPP_SOLC`                | n/a                        | solc binary to use                                                                                                                         |
| `DAPP_VERBOSE`             | n/a                        | Produce more `dapp test` output                                                                                                            |
| `DAPP_LIBRARIES`           | automatically deployed     | Library addresses to link to                                                                                                               |
| `DAPP_SKIP_BUILD`          | n/a                        | Avoid compiling this time                                                                                                                  |
| `DAPP_LINK_TEST_LIBRARIES` | `1` when testing; else `0` | Compile with libraries                                                                                                                     |
| `DAPP_VERIFY_CONTRACT`     | `yes`                      | Attempt Etherscan verification                                                                                                             |
| `DAPP_STANDARD_JSON`       | $(dapp mk-standard-json)   | [Solidity compilation options](https://docs.soliditylang.org/en/latest/using-the-compiler.html#compiler-input-and-output-json-description) |
| `DAPP_REMAPPINGS`          | $(dapp remappings)         | [Solidity remappings](https://docs.soliditylang.org/en/latest/using-the-compiler.html#path-remapping)                                      |
| `DAPP_BUILD_OPTIMIZE`      | no                         | Activate Solidity optimizer                                                                                                                |
| `DAPP_BUILD_OPTIMIZE_RUNS` | 200                        | Set the optimizer runs                                                                                                                     |

A global (always loaded) config file is located in `~/.dapprc`.
A local `.dapprc` can also be defined in your project's root, which overrides variables in the global config.

### solc version

You can specify a custom `solc` version to run within `dapp` with `dapp --use <arg>`.
If the argument is of the form `solc:x.y.z`, the appropriate solc version
will temporarily installed. 
If the argument contains a `/`, it is interpreted as a path to a solc 
binary to be used.

You can install any supported `solc` "standalone" (i.e. add it to your `$PATH`) with:
```sh
nix-env -iA solc-versions.solc_x_y_z \
  -if https://github.com/dapphub/dapptools/tarball/master
```
or
```sh
nix-env -iA solc-static-versions.solc_x_y_z \
  -if https://github.com/dapphub/dapptools/tarball/master
```

For a list of the supported `solc` versions, check
[`./nix/solc-versions.nix`](./nix/solc-versions.nix).

Versions of `solc` that haven't yet landed in nixpkgs can be found under the
`unreleased` key: `solc-versions.unreleased.solc_x_y_z`.

*(NOTE: not all versions are supported on macOS platforms.)*

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
        -v, --verbose             trace ouput for failing tests
        -vv                       trace output for all tests including passes
        --verbosity <number>      sets the verbosity to <number>
        --fuzz-runs <number>      number of times to run fuzzing tests
        --replay <string>         rerun a particular test case
        -m, --match <string>      only run test methods matching regex

    RPC options:
        --rpc                     fetch remote state via ETH_RPC_URL
        --rpc-url <url>           fetch remote state via <url>
        --rpc-block <number>      block number (latest if not specified)

    SMT options:
        --smttimeout <number>     timeout passed to the smt solver in ms (default 30000)
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

The `-v` flag prints call traces for failing tests, `-vv` for all tests.

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

If the project you want to install does not follow the typical `dapp` project structure,
you may need to configure the `DAPP_REMAPPINGS` environment variable to be able to find
it. For an example, see [this repo](https://github.com/dapp-org/radicle-contracts-tests/).

### `dapp update`

    dapp-update -- fetch all upstream lib changes
    Usage: dapp update [<lib>]

Updates a project submodule in the `lib` subdirectory.


### `dapp upgrade`

    dapp-upgrade -- pull & commit all upstream lib changes
    Usage: dapp upgrade [<lib>]

### `dapp testnet`

Spins up a geth testnet.

### `dapp verify-contract`

    dapp-verify-contract -- verify contract souce on etherscan
    Usage: dapp verify-contract <path>:<contractname> <address> [constructorArgs]

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
