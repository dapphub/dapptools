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
   pkg             use the dapp package manager
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
| `DAPPTOOLS`                | `~/.dapp/dapptools`        | Installed location of tools           |
| `DAPP_SRC`                 | `src`                      | Project Solidity source directory     |
| `DAPP_LIB`                 | `lib`                      | Directory for installed Dapp packages |
| `DAPP_OUT`                 | `out`                      | Directory for compilation artifacts   |
| `DAPP_SOLC_VERSION`        | n/a                        | Solidity compiler version to use      |
| `DAPP_VERBOSE`             | n/a                        | Produce more `dapp test` output       |
| `DAPP_SKIP_BUILD`          | n/a                        | Avoid compiling this time             |
| `DAPP_NO_HEVM`             | n/a                        | Skip HEVM tests                       |
| `DAPP_LINK_TEST_LIBRARIES` | `1` when testing; else `0` | Compile with libraries                |
| `DAPP_VERIFY_CONTRACT`     | `yes`                      | Attempt Etherscan verification        |
| `SOLC_FLAGS`               | n/a                        | Compilation flags passed to `solc`    |

A global (always loaded) config file is located in `~/.dapprc`.
A local `.dapprc` can also be defined in your project's root, which overrides variables in the global config.

## Installation

`dapp` is distrubuted as part of the [Dapp tools suite](../../README.md).

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
