# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
