# Dapp tools by DappHub [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

Hello!

This repository contains the source code for several Ethereum tools
hand-crafted and maintained by DappHub, along with dependency management, courtesy of Nix.

Contents:

- [Dapp](./src/dapp) - all you need Ethereum development tool. Build, test, debug & deploy solidity contracts.
- [Seth](./src/seth) - Ethereum CLI. Query contracts, send transactions, follow logs, slice & dice data.
- [Hevm](./src/hevm) - Ethereum evaluator. Fully complient Haskell EVM implementation.
- [Ethsign](./src/ethsign) - sign Ethereum transactions from a local keystore.

## Easy install
All tools and dependencies can be automaticaly installed by running:
```sh
curl https://dapp.tools/install | sh
```

You _may_ also need to add the nix binaries to your path:

```sh
export PATH="$PATH:$HOME/.nix-profile/bin"
```

## Manual install
For a manual install, please follow the instructions below.

### Install Nix

Make sure you are using Nix v2

    curl https://nixos.org/nix/install | sh

    # Run this or login again to use Nix
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"

### Setup Cachix (optional)

If you want to cache the binary builds across several computers, Nix
provides several ways of doing this, the most convenient being
the [Cachix](https://cachix.org) service.

We maintain a public cache which you can use by installing Cachix

    nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=

then running

    cachix use dapp

followed by restarting your Nix daemon (if on a multi-user Nix installation)

### Install tools

To install tools from this repository, you need to run

    git clone --recursive https://github.com/dapphub/dapptools $HOME/.dapp/dapptools
    nix-env -f $HOME/.dapp/dapptools -iA dapp seth solc hevm ethsign

which will build and install these tools into your Nix profile, optionally
fetching the binaries from the cache if you have configured it.

### Installing custom solc versions

For a list of the supported `solc` versions, check
[`./nix/solc-versions.nix`](./nix/solc-versions.nix).

You can specify a `solc` version to run within `dapp` with `dapp --use
solc:x.y.z test`, but you can also install `solc` "standalone" (i.e. add it to
your `$PATH`) with:

```
   $ nix-env -f $HOME/.dapp/dapptools -iA solc-versions.solc_0_5_0
```

Versions of `solc` that haven't yet landed in nixpkgs can be found under the
`unreleased` key: `solc-versions.unreleased.solc_x_y_z`. Again, refer to
[`./nix/solc/versions.nix`](./nix/solc-versions.nix) for a list of supported
unreleased versions.

NOTE: not all versions are supported on macOS platforms.

### Contributing

Instructions for adding new versions of `solc` can be found at
[`nix/solc-updates.md`](./nix/solc-updates.md)

---
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
