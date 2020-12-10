# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## [0.31.1] - 2020-12-10

## Added

- `dapp --use` can find solc versions in the nix store even if they are not present on `PATH`

## Fixed

- Corrected help text for solc version installation

## [0.31.0] - 2020-11-29

### Changed

- `dapp test --match` now matches on file path and contract name, as
  well as test name
- `dapp --use` searches directly for binaries in your path, rather than
  using `nix run`, giving a significant speed boost.

## [0.30.0] - 2020-10-31

### Added
- Support for solc 0.6.8
- Support for setting solc by path. Use `DAPP_SOLC=/path/to/bin` or
`dapp --use /path/to/solc ...` to use a custom solc.
- dapp test --cache for RPC caching, via hevm.

## [0.29.0] - 2020-08-19

### Added
- Support for solc 0.6.12 for linux

### Fixed
- Support for libraries. To test a project which includes libraries you need to use:
```sh
DAPP_BUILD_EXTRACT=1 dapp test
```

## [0.28.0] - 2020-07-13
### Added
- Support for solc 0.6.7
- The json outputted by `dapp build` will now contain `storage-layout`, if the chosen solidity version allows.


### Changed
- Support for solc 0.6.7
- Updated hevm to 0.40
- Updated `geth` to 1.9.10

### Removed
- Support for git submodules (setzer, dai-cli, chief, terra)
- `dapp pkg` functionality - dapptools is now installed and upgraded as a Nix
package only, without relying on git
- `DAPPTOOLS` environment variable, which was only needed to support the
git-based workflow above

### Deprecated
- `dapp pkg` subcommand is now a no-op, and will be removed in future
versions.

## [0.27.0] - 2020-04-23
### Added
- Integration with `hevm`s new property based testing functionality. Any test with nonzero arguments will be interpreted as a property test whose arguments are randomly generated and run `--fuzz-runs` number of times.
- Support for solc 0.5.16
- Support for solc 0.6.0
- Support for solc 0.6.6

### Changed
- `--extract` flag for `dapp build` no longer needed for using `dapp create`.
- `dapp create` will fail if it finds multiple contracts with same name, requiring full path instead.

## [0.26.0]
### Added
- Support for solc 0.5.15

### Fixed
- `dapp address` and `dapp create` now returns checksummed addresses

### Changed
- `dapp address` returns address with `0x` prefix.
- Default to solc 0.5.15
- Since solc 0.5.15, shared libraries are not built anymore, which might break
  certain rare usecases


## [0.25.0] - 2019-08-02
### Fixed
- Correctly load testnet snapshots which were saved with multiple accounts
### Changed
- `dapp address` is now implemented in bash rather than depending on
  ethereumjs-util
- Support hevm RPC - options added to `dapp test` and `dapp debug` to
  allow unit tests to reference remote contracts.

## [0.24.0] - 2019-07-09
### Added
- Support for solc 0.5.10

### Changed
- Default to solc 0.5.10

## [0.23.0] - 2019-07-08
### Changed
- Faster `dapp build`, with a single compilation pass. Consolidates solc
  output into a single `out/dapp.sol.json`, which may break some
  workflows. `dapp build --extract` can be used to extract .abi, .bin,
  .bin-runtime files from the json into `out/`.
- The version of `hevm` is now shown in `dapp --version`.

## [0.22.0] - 2019-06-21
### Changed
- geth upgraded to 1.8.27

## [0.21.0] - 2019-06-21
### Added
- Support for solc 0.5.9

### Changed
- Default to solc 0.5.9

## [0.20.0] - 2019-05-23
### Added
- Support for solc 0.5.8

## [0.19.0] - 2019-05-09
## Changed
- The default output of `dapp test` is now to only show assertion failures.
  Other output modes can be accessed with `-v` and `-vv`.

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
[0.19.0]: https://github.com/dapphub/dapptools/tree/dapp/0.19.0
[0.20.0]: https://github.com/dapphub/dapptools/tree/dapp/0.20.0
[0.21.0]: https://github.com/dapphub/dapptools/tree/dapp/0.21.0
[0.22.0]: https://github.com/dapphub/dapptools/tree/dapp/0.22.0
[0.23.0]: https://github.com/dapphub/dapptools/tree/dapp/0.23.0
[0.24.0]: https://github.com/dapphub/dapptools/tree/dapp/0.24.0
[0.25.0]: https://github.com/dapphub/dapptools/tree/dapp/0.25.0
[0.26.0]: https://github.com/dapphub/dapptools/tree/dapp/0.26.0
[0.27.0]: https://github.com/dapphub/dapptools/tree/dapp/0.27.0
