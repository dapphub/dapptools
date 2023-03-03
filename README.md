# Dapp tools by DappHub [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

Hello!

`dapptools` is a suite of Ethereum focused CLI tools following the Unix design philosophy,
favoring composability, configurability and extensibility.

This repository contains the source code for several programs
hand-crafted and maintained by DappHub, along with dependency management, courtesy of Nix.

- [dapp](./src/dapp) - All you need Ethereum development tool. Build, test, fuzz, formally verify, debug & deploy solidity contracts.
- [seth](./src/seth) - Ethereum CLI. Query contracts, send transactions, follow logs, slice & dice data.
- [ethsign](./src/ethsign) - Sign Ethereum transactions from a local keystore or hardware wallet.

## Development Status

dapptools is currently in a stage of clandestine development where support for the casual user may
be deprived. The software can now be considered free as in free puppy. Users seeking guidance can
explore using foundry as an alternative

hevm was previously maintained as a part of this repository, but has since been forked out by the
formal methods team at the ethereum foundation, and is now developed at
[ethereum/hevm](https://github.com/ethereum/hevm).

## Installation

Install Nix if you haven't already ([instructions](https://nixos.org/download.html)). Then install dapptools:

### With flakes

```
nix profile install github:dapphub/dapptools#{dapp,ethsign,hevm,seth,solc}
```

Nix will offer to use the dapptools binary cache, which will speed up installs,
but requires you to trust both us and the Cachix infrastructure.

### Legacy

```
curl https://dapp.tools/install | sh
```

This configures the dapphub binary cache and installs the `dapp`, `solc`, `seth` and `hevm` executables.

**NOTE:** Arm support in the GHC haskell compiler is still fairly bleeding edge, until this
situation stabilises, users of M1 macs must run dapptools (and the installer!) under rosetta 2 (i.e.
as an emulated x86 program). Make sure `/etc/nix/nix.conf` contains `system = x86_64-darwin`.

You can also install an individual tool with:

```sh
nix-env -iA <tool> -f $(curl -sS https://api.github.com/repos/dapphub/dapptools/releases/latest | jq -r .tarball_url)
```

If you instead want to build from `master`, change the url to `https://github.com/dapphub/dapptools/archive/master.tar.gz`.

## Getting started

For more information about the tools, consult the individual README pages:

- [seth](./src/seth/README.md)
- [dapp](./src/dapp/README.md)
- [ethsign](./src/ethsign/README.md)

or use the `--help` flag for any tool.

We're also happy to answer any questions at https://dapphub.chat/.

## Examples

Deploy a 'Hello World' contract and call it:
```sh
export ETH_RPC_URL=https://mainnet.infura.io/v3/$YOUR_API_KEY
export ETH_FROM=$YOUR_ADDRESS
echo 'contract Hello { function hi() public pure returns(string memory) {return "Hello, World!";}}' | solc --bin -o . --overwrite -
HELLO=$(seth send --create $(<Hello.bin))
seth call $HELLO "hi()(string)"
```

Debug the first transaction of the latest block in the interactive debugger:
```sh
export ETH_RPC_URL=https://mainnet.infura.io/v3/$YOUR_API_KEY
seth run-tx $(seth block latest transactions | jq .'[0]' -r) --debug
```

If Vitalik's next transaction were a contract deployment, calculate the address it would be deployed at:
```
export ETH_RPC_URL=https://mainnet.infura.io/v3/$YOUR_API_KEY
dapp address 0xab5801a7d398351b8be11c439e05c5b3259aec9b $(seth nonce 0xab5801a7d398351b8be11c439e05c5b3259aec9b)
```

## Contributing

Contributions are always welcome! You may be interested in the
[architecture](./ARCHITECTURE.md) of this repository.

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
