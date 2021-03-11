# Dapp [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

`dapp` is a tool for building, testing and deploying smart contracts from the comfort of the command line.

As opposed to other tools, it does not use `rpc` to execute transactions. Instead,
it invokes the `hevm` cli directly. This is faster, and allows for a lot of flexibility
that isn't available in `rpc`, such as [fuzz testing](#dapp-test-flags), symbolic execution, or [cheat codes to modify mainnet state](../hevm/README.md#cheat-codes).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Dapp *](#dapp-)
  - [Usage](#usage)
  - [Configuration](#configuration)
  - [Installation](#installation)
  - [Using custom solc versions](#using-custom-solc-versions)
  - [Using dapp test](#using-dapp-test)
    - [`dapp test` flags:](#dapp-test-flags)
    - [Alternative install](#alternative-install)
    - [Docker](#docker)

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
the value a parameter:
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

### Symbolically execution tests

While property based testing runs each function repeatedly with new input values, symbolic execution tries leaves these 
values symbolic and tries to explore each possible execution path. This gives a stronger guarantee and is powerful than
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

## Configuration

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

## Installation

`dapp` is distrubuted as part of the [Dapp tools suite](../../README.md).

## Using custom solc versions

You can specify a custom `solc` version to run within `dapp` with `dapp --use
solc:x.y.z test`, but you can also install any supported `solc` "standalone"
(i.e. add it to your `$PATH`) with:

```
nix-env -iA solc-versions.solc_x_y_z \
  -if https://github.com/dapphub/dapptools/tarball/master
```

*(NOTE: if you haven't installed dapptools with the one-line installer, you'll
have to manually pass substituters in the command above, or configure Cachix
manually, to avoid compilation)*

For a list of the supported `solc` versions, check
[`./nix/solc-versions.nix`](./nix/solc-versions.nix).

Versions of `solc` that haven't yet landed in nixpkgs can be found under the
`unreleased` key: `solc-versions.unreleased.solc_x_y_z`.

*(NOTE: not all versions are supported on macOS platforms.)*

## Using dapp test

dapp tests are written in Solidity using the `ds-test` module. To install it, run
```sh
dapp install ds-test
```

Every contract which inherits from `DSTest` will be treated as a test contract, and will be assumed to have a public `setUp()` function, which will be run before every test.

To write a test function, simply prefix the function name by `test`.

Example:
```sol
import "ds-test/test.sol";

contract A {
  function isEven(uint x) returns (bool) {
    return x % 2 == 0;
  }
}

contract C is DSTest {
  A a;
  function setUp() public {
    a = new A();
  }
  function test_isEven() external {
    assertTrue(a.isEven(4));
  }
}
```

Run `dapp test` to run the test suite.


### `dapp test` flags:

If you provide `--rpc-url`, state will be fetched via rpc. Local changes take priority.

You can configure the testing environment using [hevm specific environment variables](https://github.com/dapphub/dapptools/tree/master/src/hevm#environment-variables).

To modify local state even more, you can use [hevm cheat codes](https://github.com/dapphub/dapptools/tree/master/src/hevm#cheat-codes).

If your test function takes arguments, they will be randomly instantiated and the function will be run multiple times.

The number of times run is configurable using `--fuzz-runs`.

To step through a test in `hevm` interactive debugger, use `dapp debug`.

`dapp --use` allows for configuration of the solidity version.
```
dapp --use solc:0.5.12 test
```

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

### Alternative install
If you don't want to use Nix, we provide an alternative installation mechanism using `make` below.

Please make sure you have:

* [`solc`](https://solidity.readthedocs.io/en/develop/installing-solidity.html)
* Bash 4

and then run:

```
   make link                  install dapp(1) into /usr/local
   make uninstall             uninstall dapp(1) from /usr/local
```


### Docker

The provided `Dockerfile` is based on the `node` image.

```
docker build -t dapp .                build the Docker image
docker run -it -v `pwd`:/src dapp     run `dapp test' on the current directory
```
