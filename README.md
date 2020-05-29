# Dapp tools by DappHub [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

Hello!

This repository contains the source code for several Ethereum tools
hand-crafted and maintained by DappHub, along with dependency management, courtesy of Nix.

Contents:

- [Dapp](./src/dapp) - all you need Ethereum development tool. Build, test, debug & deploy solidity contracts.
- [Seth](./src/seth) - Ethereum CLI. Query contracts, send transactions, follow logs, slice & dice data.
- [Hevm](./src/hevm) - Ethereum evaluator. Fully complient Haskell EVM implementation.
- [Ethsign](./src/ethsign) - sign Ethereum transactions from a local keystore.

## Instalation

Install `nix` if you haven't already:

```sh
curl https://nixos.org/nix/install | sh

# Run this or login again to use Nix
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
```

Then install dapptools:

```
nix-env -iA seth dapp hevm \
  -if https://github.com/dapphub/dapptools/tarball/master \
  --substituters https://dapp.cachix.org \
  --trusted-public-keys dapp.cachix.org-1:9GJt9Ja8IQwR7YW/aF0QvCa6OmjGmsKoZIist0dG+Rs=
```

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
