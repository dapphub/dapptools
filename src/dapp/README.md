# Dapp

[![Docs](https://img.shields.io/badge/view%20docs-readthedocs-blue.svg?style=flat-square)](https://dapp.readthedocs.io/en/latest/)
[![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

## Installation

`dapp` can be installed as part of the [dapp.tools suite](../../README.md).

If you don't want to use nix, we provide an alternative installation mechanism
using `make` below.


Please make sure you have:

* [`solc`](https://solidity.readthedocs.io/en/develop/installing-solidity.html)
* Bash 4

and then run:

```
   make link                  install dapp(1) into /usr/local
   make uninstall             uninstall dapp(1) from /usr/local
```

## Usage

```
   dapp init                  create a new dapp in the current directory

   dapp build                 compile your dapp's source code
   dapp test                  run your dapp's test suite

   dapp install <pkg>         install the <pkg> package (e.g. `ds-auth')
   dapp upgrade <pkg>         upgrade the <pkg> package
   dapp uninstall <pkg>       uninstall the <pkg> package
   
   dapp testnet               spin up an ethereum testnet
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


## Docker

The provided `Dockerfile` is based on the `node` image.


```
docker build -t dapp .                build the Docker image
docker run -it -v `pwd`:/src dapp     run `dapp test' on the current directory
```
