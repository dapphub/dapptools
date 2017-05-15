# Dapp

## Installation

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
```

## Docker

The provided `Dockerfile` is based on the `node` image.


```
docker build -t dapp .                build the Docker image
docker run -it -v `pwd`:/src dapp     run `dapp test' on the current directory
```
