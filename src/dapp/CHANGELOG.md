# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Support for solc 0.5.1
- Introduce a new environment variable `DAPP_SOLC_VERSION`. Overrides default `solc` used by `dapp`. Can be used with `export DAPP_SOLC_VERSION=0.5.0` instead of `dapp --use solc:0.5.0 [command]` for every command.

## [0.9.1] - 2018-12-13
### Added
- Pass `--verbose` flag to `hevm`

## [0.9.0] - 2018-12-04

From this release on, we will document changes, additions and removals in this
changelog.

[Unreleased]: https://github.com/dapphub/dapptools/compare/dapp/0.9.1...HEAD
[0.9.1]: https://github.com/dapphub/dapptools/tree/dapp/0.9.1
[0.9.0]: https://github.com/dapphub/dapptools/tree/dapp/0.9.0
