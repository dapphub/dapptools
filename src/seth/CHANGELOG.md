# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## Changed

- updated `nixpkgs` to the `20.09` channel

### Fixed

- `seth calldata` returns correct abiencoding

## [0.10.0] - 2021-01-26

### Changed

- `seth combined-json` was renamed to `seth-solc` and invokes `solc`
  using the `--standard-json` input.
- `seth bundle-source` correctly interprets etherscan sources using
  standard json
- the `--gas-price` argument can optionally accept a `gwei` suffix
  i.e. `seth call --gas-price 100gwei ...`

## [0.9.4] - 2020-12-10

### Added

- `seth --use` can find solc versions in the nix store even if they are not present on `PATH`

### Fixed

- Correct help text for `seth --use`

## [0.9.3] - 2020-11-29

- `seth --use` searches directly for binaries in your path, rather than
  using `nix run`, giving a significant speed boost.

## [0.9.2] - 2020-10-31

### Added
- Updated `dapp` and `hevm`

## [0.9.1] - 2020-08-19

### Added
- New commands:
  - `seth source <address>` fetches the contract source from etherscan
  - `seth bundle-source <address>` fetches contract source and compiles to combined json
  - `seth run-tx <tx-hash> [--debug, --source <file>, --state <repository>]`,
  set hevm options according to the options given and `hevm exec`
  - `seth debug <tx-hash> [--no-src]` downloads etherscan source, executes previous txs in block and enters into an hevm interactive session for the given tx. `--no-src` skips the first step.
  - `seth --{max-uint,max-int,min-int}` print the largest numbers (in hex) of a given bitsize
  - `seth call --hevm` executes a call via hevm, rather than the RPC
  - `seth call --debug` executes a call interactively via hevm
  - `seth --verbose` prints some seth debugging information to stderr

- `seth block` can now be passed the `--full` option, which returns the full block.
- hexdata can be concatenated with `:`, e.g. '0xaa:0xbb' will be read as '0xaabb'.

### Changed
- `seth --to-{hex,wei,word,address,dec,int256,ascii,fix,uint256}` and
  `seth --from-{ascii,wei}` can now read values from stdin.
- `seth call` now accepts empty calldata and also create transactions
  via `--create`.
- `seth --abi-function-json` no longer returns a singleton list, but rather the JSON object it contained.
- Updated hevm to 0.41.0

## [0.9.0] - 2020-05-25

### Changed
- `hevm` is now a dependency of seth.
- `seth calldata` now implemented using hevm instead of `ether.js`, fixes several bugs.
- Bytestring arguments to seth calldata must not be enclosed in double quotes.

## [0.8.4] - 2020-02-21

### Changed
- `seth {--to-uint256,--to-int256,--to-bytes32}` now returns hex string with `0x` prefix.
- Removed ethabi dependency in favor of (already present) `ethers.js`.
- `0x` prefix is strictly required for bytestrings while using `seth calldata`.
- When giving array arguments to `seth calldata`, each bytestring must be enclosed in double quotes.

### Added
- `seth --calldata-decode` command [#338](https://github.com/dapphub/dapptools/pull/338)

## [0.8.3] - 2019-08-08
### Fixed
 - ABI JSON blob creation would fail for some solc reference types, e.g. `bytes
 memory`, since solc 0.5.0. solc was removed as a dependency and replaced with
 ethers.js.

## [0.8.2] - 2019-05-23
### Fixed
- Regression in `seth send` when `ETH_RPC_ACCOUNTS` was set

## [0.8.1] - 2019-05-7
### Fixed
- `seth call`, `seth send` and `seth estimate` were broken due to an update in
  [0.8.0], which has been reverted

## [0.8.0] - 2019-05-02
### Added
- Support for `goerli` and `kotti` testnets

### Fixed
- `seth logs-etherscan` now correctly handles its fourth optional argument, the `ilk`

## [0.7.0] - 2018-12-14

From this release on, changes, additions and removals will be documented in this
changelog.

### Changed
- `seth abi`: use the current `solc` version in the `pragma` directive.
[#99](https://github.com/dapphub/dapptools/pull/99)

[0.7.0]: https://github.com/dapphub/dapptools/tree/seth/0.7.0
[0.8.0]: https://github.com/dapphub/dapptools/tree/seth/0.8.0
[0.8.1]: https://github.com/dapphub/dapptools/tree/seth/0.8.1
[0.8.2]: https://github.com/dapphub/dapptools/tree/seth/0.8.2
