# Dapp [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

Ethereum development, dapphub-style.

## Usage

```
Usage: dapp <command> [<args>]
   or: dapp help <command>

Build, test, deploy and transact with Ethereum contracts from the comfort of your command line.

Commands:

   address         determine address of newly generated contract
   build           compile the source code
   clean           remove compiled source directory
   clone           clone a github repo
   create          deploy a compiled contract (--verify on Etherscan)
   debug           start an interactive debugger for unit tests (hevm)
   help            print help about dapp(1) or one of its subcommands
   init            bootstrap a new dapp
   install         install a smart contract library
   test            run the test suite
   testnet         launch a testnet
   uninstall       remove a smart contract library
   update          fetch all upstream lib changes
   upgrade         pull & commit all upstream lib changes

```

## Configuration

These variables can be set at the prompt or in a `.dapprc` file.

|          Variable          |          Default           |               Synopsis                |
|----------------------------|----------------------------|---------------------------------------|
| `DAPP_SRC`                 | `src`                      | Project Solidity source directory     |
| `DAPP_LIB`                 | `lib`                      | Directory for installed Dapp packages |
| `DAPP_OUT`                 | `out`                      | Directory for compilation artifacts   |
| `DAPP_SOLC_VERSION`        | n/a                        | Solidity compiler version to use      |
| `DAPP_VERBOSE`             | n/a                        | Produce more `dapp test` output       |
| `DAPP_SKIP_BUILD`          | n/a                        | Avoid compiling this time             |
| `DAPP_LINK_TEST_LIBRARIES` | `1` when testing; else `0` | Compile with libraries                |
| `DAPP_VERIFY_CONTRACT`     | `yes`                      | Attempt Etherscan verification        |
| `SOLC_FLAGS`               | n/a                        | Compilation flags passed to `solc`    |

A global (always loaded) config file is located in `~/.dapprc`.
A local `.dapprc` can also be defined in your project's root, which overrides variables in the global config.

## Installation

`dapp` is distrubuted as part of the [Dapp tools suite](../../README.md).

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
