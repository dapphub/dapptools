# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added
- New commands: 
  - `seth source <address>` fetches the contract source from etherscan
  - `seth bundle-source <address>` fetches contract source and compiles to combined json
  - `seth run-tx <tx-hash> [--debug, --source <file>, --state <repository>]`,
  set hevm options according to the options given and `hevm exec`
  - `seth debug <tx-hash> [--no-src]` download etherscan source, execute previous txs in block and enter into an hevm interactive mode for the given tx. `--no-src` skips the first step.

- `seth block` can now be passed `--full` option, which returns the full block.


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
