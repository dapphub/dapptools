# Dapp tools by DappHub [![Chat](https://img.shields.io/badge/community-chat-blue.svg?style=flat-square)](https://dapphub.chat)

Hello!

`dapptools` is a suite of Ethereum focused CLI tools following the unix design philosophy,
favoring composability, configurability and extensibility.

This repository contains the source code for several programs
hand-crafted and maintained by DappHub, along with dependency management, courtesy of Nix.

- [dapp](./src/dapp) - all you need Ethereum development tool. Build, test, fuzz, formally verify, debug & deploy solidity contracts.
- [seth](./src/seth) - Ethereum CLI. Query contracts, send transactions, follow logs, slice & dice data.
- [hevm](./src/hevm) - Testing oriented EVM implementation. Debug, fuzz, or symbolically execute code against local or mainnet state.
- [ethsign](./src/ethsign) - sign Ethereum transactions from a local keystore or hardware wallet.

## Installation

Install Nix if you haven't already:

```sh
# user must be in sudoers
curl -L https://nixos.org/nix/install | sh

# Run this or login again to use Nix
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
```

Then install dapptools:

```
curl https://dapp.tools/install | sh
```

This configures the dapphub binary cache and installs the `dapp`, `solc`, `seth` and `hevm` executables. 

You can also install an individual tool with 
```sh
nix-env -iA <tool> -f $(curl -sS https://api.github.com/repos/dapphub/dapptools/releases/latest | jq -r .tarball_url)
```

If you instead want to build from `master`, change the url to `https://github.com/dapphub/dapptools/archive/master.tar.gz`.

## Getting started

For more information about the tools, consult the individual README pages:

[seth](./src/seth/README.md)
[dapp](./src/dapp/README.md)
[hevm](./src/dapp/README.md)
[ethsign](./src/ethsign/README.md)

or use the `--help` flag for any tool.

We're also happy to answer any questions at https://dapphub.chat/.

## Examples

Deploy a Hello world contract and call it:
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

If Vitaliks next transaction were a contract deployment, calculate the address it would be deployed at:
```
export ETH_RPC_URL=https://mainnet.infura.io/v3/$YOUR_API_KEY
dapp address 0xab5801a7d398351b8be11c439e05c5b3259aec9b $(seth nonce 0xab5801a7d398351b8be11c439e05c5b3259aec9b)
```

Symbolically explore the possible execution paths of a call to `dai.transfer(address,uint)`:
```sh
seth bundle-source 0x6b175474e89094c44da98b954eedeac495271d0f > daisrc.json && \
hevm symbolic --address 0x6b175474e89094c44da98b954eedeac495271d0f --rpc $ETH_RPC_URL  --debug --sig "transfer(address,uint256)" --json-file daisrc.json
```

## Contributing

Contributions are always welcome! You may be interested in the 
[architecture](./ARCHITECTURE.md) of this repository.

---
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
