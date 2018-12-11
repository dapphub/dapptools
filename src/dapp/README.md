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


## Docker

The provided `Dockerfile` is based on the `node` image.


```
docker build -t dapp .                build the Docker image
docker run -it -v `pwd`:/src dapp     run `dapp test' on the current directory
```
