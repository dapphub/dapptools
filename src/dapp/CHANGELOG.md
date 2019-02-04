# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.12.0] - 2018-02-04
### Changed
- Default solc to 0.5.3
- Updated go-ethereum to v1.8.22 (Constantinople)

## [0.11.0] - 2018-02-02
### Added
- Support for solc 0.5.3

## [0.10.0] - 2018-01-24
### Changed
- Default to running solc 0.5.2

## [0.9.2] - 2018-01-19
### Added
- Support for solc 0.5.1 and 0.5.2
- Introduced a new environment variable `DAPP_SOLC_VERSION`, that overrides the
default `solc` used by `dapp`. Can be used with `export DAPP_SOLC_VERSION=0.5.0`
instead of `dapp --use solc:0.5.0 [command]` for every command.

## [0.9.1] - 2018-12-13
### Added
- Pass `--verbose` flag to `hevm`

## [0.9.0] - 2018-12-04

From this release on, we will document changes, additions and removals in this
changelog.

[Unreleased]: https://github.com/dapphub/dapptools/compare/dapp/0.9.1...HEAD
[0.9.0]: https://github.com/dapphub/dapptools/tree/dapp/0.9.0
[0.9.1]: https://github.com/dapphub/dapptools/tree/dapp/0.9.1
[0.9.2]: https://github.com/dapphub/dapptools/tree/dapp/0.9.2
[0.10.0]: https://github.com/dapphub/dapptools/tree/dapp/0.10.0
[0.11.0]: https://github.com/dapphub/dapptools/tree/dapp/0.11.0
[0.12.0]: https://github.com/dapphub/dapptools/tree/dapp/0.12.0
