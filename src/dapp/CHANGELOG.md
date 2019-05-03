# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.18.1] - 2019-05-03
### Fixed
- Revert the change to the output of `dapp create`, because it created problems
  when concatenating scripts.

## [0.18.0] - 2019-05-02
### Changed
- The return value of `dapp debug` in case of error is `1`, rather than `-1`
- Improved printing of the address of the deployed contract in `dapp create`

## [0.17.0] - 2019-03-28
### Added
- Support for solc 0.5.7

## [0.16.0] - 2019-03-20
### Added
- Support for solc 0.5.6

### Changed
- Default to solc 0.5.6

## [0.15.1] - 2019-03-05
### Fixed
- The version returned by `dapp --version` had not been updated from 0.14.0 to 0.15.0

## [0.15.0] - 2019-03-04
### Fixed
- The default solc had been incorrectly re-set to 0.5.3 - it's 0.5.4 again now.

## [0.14.0] - 2019-02-22

### Fixed
- Fix missing runtime dependency to `gnumake` in `dapp`.

## [0.13.0] - 2018-02-04
### Added
- Support for solc 0.5.4

### Changed
- Default solc to 0.5.4

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

[0.9.0]: https://github.com/dapphub/dapptools/tree/dapp/0.9.0
[0.9.1]: https://github.com/dapphub/dapptools/tree/dapp/0.9.1
[0.9.2]: https://github.com/dapphub/dapptools/tree/dapp/0.9.2
[0.10.0]: https://github.com/dapphub/dapptools/tree/dapp/0.10.0
[0.11.0]: https://github.com/dapphub/dapptools/tree/dapp/0.11.0
[0.12.0]: https://github.com/dapphub/dapptools/tree/dapp/0.12.0
[0.13.0]: https://github.com/dapphub/dapptools/tree/dapp/0.13.0
[0.14.0]: https://github.com/dapphub/dapptools/tree/dapp/0.14.0
[0.15.0]: https://github.com/dapphub/dapptools/tree/dapp/0.15.0
[0.15.1]: https://github.com/dapphub/dapptools/tree/dapp/0.15.1
[0.16.0]: https://github.com/dapphub/dapptools/tree/dapp/0.16.0
[0.17.0]: https://github.com/dapphub/dapptools/tree/dapp/0.17.0
[0.18.0]: https://github.com/dapphub/dapptools/tree/dapp/0.18.0
[0.18.1]: https://github.com/dapphub/dapptools/tree/dapp/0.18.1
