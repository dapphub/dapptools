# hevm changelog

## 0.39 - unreleased
 - Exposes abi encoding to cli
 - Added cheat code `hevm.store(address a, bytes32 location, bytes32 value)`
 - Removes `ExecMode`, always running as `ExecuteAsBlockchainTest`. This means that `hevm exec` now finalizes transactions as well.
 - `--code` is now entirely optional. Not supplying it returns an empty contract, or whatever is stored in `--state`.

## 0.38 - 2020-04-23
 - Exposes metadata stripping of bytecode to the cli: `hevm strip-metadata --code X`. [357](https://github.com/dapphub/dapptools/pull/357).
 - Fixes a bug in the srcmap parsing introduced in 0.37 [356](https://github.com/dapphub/dapptools/pull/356).
 - Fixes a bug in the abi-encoding of `bytes` with size > 32[358](https://github.com/dapphub/dapptools/pull/358).

## 0.37 - 2020-03-24
 - Sourcemap parser now admits `solc-0.6.0` compiled `.sol.json` files.

## 0.36 - 2020-01-07
 - Implement Istanbul support [318](https://github.com/dapphub/dapptools/pull/318)
 - Fix a bug introduced in [280](https://github.com/dapphub/dapptools/pull/280) of rlp encoding of transactions and sender address [320](https://github.com/dapphub/dapptools/pull/320/).
 - Make InvalidTx a fatal error for vm tests and ci.
 - Suport property based testing in unit tests. [313](https://github.com/dapphub/dapptools/pull/313) Arguments to test functions are randomly generated based on the function abi. Fuzz tests are not present in the graphical debugger.
 - Added flags `--replay` and `--fuzz-run` to `hevm dapp-test`, allowing for particular fuzz run cases to be rerun, or for configuration of how many fuzz tests are run.
 - Correct gas readouts for unit tests
 - Prevent crash when trying to jump to next source code point if source code is missing

## 0.35 - 2019-11-02
 - Merkle Patricia trie support [280](https://github.com/dapphub/dapptools/pull/280)
 - RLP encoding and decoding functions [280](https://github.com/dapphub/dapptools/pull/280)
 - Extended support for Solidity ABI encoding [259](https://github.com/dapphub/dapptools/pull/259)
 - Bug fixes surrounding unit tests and gas accounting (https://github.com/dapphub/dapptools/commit/574ef401d3e744f2dcf994da056810cf69ef84fe, https://github.com/dapphub/dapptools/commit/5257574dd9df14edc29410786b75e9fb9c59069f)

## 0.34 - 2019-08-28
 - handle new solc bzzr metadata in codehash for source map
 - show VM hex outputs as hexadecimal
 - rpc defaults to latest block
 - `hevm interactive`:
  - fix rpc fetch
  - scrollable memory pane
 - Fix regression in VMTest compliance.
 - `hevm exec` ergonomics:
  - Allow code/calldata prefixed with 0x
  - create transactions with specific caller nonce
  - interactive help pane
  - memory pane scrolling

## 0.33 - 2019-08-06
 - Full compliance with the [General State Tests][245] (with the
   BlockchainTest format), using the Yellow and Jello papers as
   reference, for Constantinople Fix (aka Petersburg). Including:
  - full precompile support
  - correct substate accounting, including touched accounts,
    selfdestructs and refunds
  - memory read/write semantics
  - many gas cost corrections
 - Show more information for non solc bytecode in interactive view
   (trace and storage)
 - Help text for all cli options
 - Enable `--debug` flag in `hevm dapp-test`

[245]: https://github.com/dapphub/dapptools/pull/245

## 0.32 - 2019-06-14
 - Fix dapp-test [nonce initialisation bug][224]

[224]: https://github.com/dapphub/dapptools/pull/224

## 0.31 - 2019-05-29
 - Precompiles: SHA256, RIPEMD, IDENTITY, MODEXP, ECADD, ECMUL,
   ECPAIRING, MODEXP
 - Show the hevm version with `hevm version`
 - Interactive mode:
  - no longer exits on reaching halt
  - new shortcuts: 'a' / 'e' for start / end
  - allow returning to test picker screen
 - Exact integer formatting in dapp-test and tty

## 0.30 - 2019-05-09
 - Adjustable verbosity level for `dapp-test` with `--verbose={0,1,2}`
 - Working stack build

## 0.29 - 2019-04-03
 - Significant jump in compliance with client tests
 - Add support for running GeneralStateTests

## 0.28 - 2019-03-22
 - Fix delegatecall gas metering, as reported in
   https://github.com/dapphub/dapptools/issues/34

## 0.27 - 2019-02-06
 - Fix [hevm flatten issue](https://github.com/dapphub/dapptools/issues/127)
   related to SemVer ranges in Solidity version pragmas

## 0.26 - 2019-02-05
 - Format Solidity Error(string) messages in trace

## 0.25 - 2019-02-04
 - Add SHL, SHR and SAR opcodes

## 0.24 - 2019-01-23
 - Fix STATICCALL for precompiled contracts
 - Assume Solidity 0.5.2 in tests

## 0.23 - 2018-12-12
 - Show passing test traces with --verbose flag

## 0.22 - 2018-11-13
 - Simple memory view in TTY

## 0.21 - 2018-10-29
 - Fix Hackage package by including C header files for ethjet

## 0.20 - 2018-10-27
 - Parse constructor inputs from Solidity AST

## 0.19 - 2018-10-09
 - Enable experimental 'cheat' address, allowing for modification of the
   EVM environment from within the tests. Currently just the block
   timestamp can be adjusted.

## 0.18 - 2018-10-09
 - Fix [duplicate address bug](https://github.com/dapphub/dapptools/issues/70)

## 0.17 - 2018-10-05
 - Semigroup/Monoid fix

## 0.16 - 2018-09-19
 - Move ethjet into hevm

## [0.15] - 2018-05-09
 - Fix SDIV/SMOD definitions for extreme case

## [0.14.1] - 2018-04-17
 - Improve PC display in TTY

## [0.14] - 2018-03-08
 - Implement STATICCALL

## [0.13] - 2018-02-28
 - Require specific block number for RPC debugging
 - Implement RETURNDATACOPY and RETURNDATASIZE
 - Fix bug where created contracts didn't get their balance

## [0.12.3] - 2017-12-19
 - More useful RPC debugging because we strip the entire BZZR metadata

## [0.12.2] - 2017-12-17
 - Experimental new ecrecover implementation via libethjet
 - Correct error checking for setUp() invocations

## [0.12.1] - 2017-11-28
 - Test name regex matching via --match
 - Fixed source map parsing bug when used with solc --optimize
 - TTY: fix a padding-related display glitch

## [0.12] - 2017-11-14
 - Use 13 different environment variables to control block parameters
   for unit testing, e.g. block number, timestamp, initial balance, etc.

   Full list:

     - `DAPP_TEST_ADDRESS`
     - `DAPP_TEST_CALLER`
     - `DAPP_TEST_ORIGIN`
     - `DAPP_TEST_GAS_CREATE`
     - `DAPP_TEST_GAS_CALL`
     - `DAPP_TEST_BALANCE_CREATE`
     - `DAPP_TEST_BALANCE_CALL`
     - `DAPP_TEST_COINBASE`
     - `DAPP_TEST_NUMBER`
     - `DAPP_TEST_TIMESTAMP`
     - `DAPP_TEST_GAS_LIMIT`
     - `DAPP_TEST_GAS_PRICE`
     - `DAPP_TEST_DIFFICULTY`

## [0.11.5] - 2017-11-14
 - Use --state with --exec --debug

## [0.11.4] - 2017-11-12
 - Fix bug when unit test contract has creations in constructor

## [0.11.3] - 2017-11-08
 - Fix array support in ABI module

## [0.11.2] - 2017-11-04
 - TTY: show a help bar with key bindings at the bottom

## [0.11.1] - 2017-11-02
 - TTY: fix a display glitch
 - TTY: improve display of ABI hashes on the stack

## [0.11] - 2017-10-31
 - Add "hevm flatten" for Etherscan-ish source code concatenation
 - Simplify code by removing concrete/symbolic machine abstraction

## [0.10.9] - 2017-10-23
 - Fix bugs in ABI formatting

## [0.10.7] - 2017-10-19
 - Fix library linking bug
 - Fix gas consumption of DELEGATECALL
 - Better error tracing
 - Experimental "contract browser" (stupid list of addresses)

## [0.10.6] - 2017-10-19
 - Enable library linking for unit tests and debugger
 - Use the same default gas/balance values as `ethrun`

## [0.10.5] - 2017-10-17
 - Better trace output including arguments and return values
 - Proof of concept coverage analysis via `dapp-test --coverage`

## [0.10] - 2017-10-10
 - Enable new trace output by default for failing tests
 - Exit with failure code from test runner when tests fail
 - More fixes to improve Ethereum test suite compliance

## [0.9.5] - 2017-10-06
 - Prototype of new trace output with `hevm dapp-test --verbose`
 - Nicer trace tree in the TTY debugger
 - Many fixes to improve Ethereum test suite compliance

## [0.9] - 2017-09-29
 - Integrates with live chains via RPC (read-only)
 - Exposes a special contract address with test-related functionality (time warp)

## [0.8.5] - 2017-09-22
 - Renames `hevm` from its maiden name `hsevm` :sparkles:

## [0.8] - 2017-09-21
 - Implements gas metering (Metropolis rules by default)
 - Shows gas counter in the terminal interface
 - Enables debugger for consensus test executions
 - Consensus test runner script with HTML reporting
 - Passes 564 of the `VMTests`; fails 115 (see [0.8 test report])
 - Command line options for specifying initial gas amounts and balances
 - Improved TTY UI layout

## [0.7] - 2017-09-07
 - Can save and load contract states to disk using a Git-backed store (only `--exec`)
 - Can debug raw EVM bytecode using `exec --debug`
 - Fixes `exec --value`
 - Has smarter defaults for command line when running tests or debugging
 - Fixes bug with `MSIZE` in `CALL` context

## [0.6.5] - 2017-09-01
 - Fixes `exec` with regards to exit codes and error messages

## [0.6.1] - 2017-08-03
 - TTY: Adds command `C-n` in TTY for "stepping over"

## [0.6] - 2017-08-03
 - TTY: Adds second line to stack entries with humanized formatting
 - TTY: Gets rid of the separate log pane in favor of a unified trace pane

## [0.5] - 2017-08-02
 - TTY: Adds `p` command for stepping backwards
 - Adds ability to track origins of stack and heap words
 - Tracks Keccak preimage for words that come from the `SHA3` instruction

## [0.4] - 2017-07-31
 - Parallelizes unit test runner
 - Improves speed by changing representation of memory
 - Internal refactoring for future support of symbolic execution
 - Adds logs to the trace pane

## [0.3.2] - 2017-06-17
 - Adds `REVERT` opcode
 - Sets `TIMESTAMP` value to `1` in unit tests

## [0.3.0] - 2017-06-14
 - Reverts contract state after `CALL` fails
 - Improves test runner console output

## [0.2.0] - 2017-06-13
 - Fixes bug in `CALL`

## [0.1.0.1] - 2017-03-31
 - Highlights Solidity exactly on character level
 - Adds `N` command for stepping by Solidity source position instead of by opcode

## 0.1.0.0 - 2017-03-29
 - First release

[0.8 test report]: https://hydra.dapp.tools/build/135/download/1/index.html

[0.12]: https://github.com/dapphub/hevm/compare/0.11.5...0.12
[0.11.5]: https://github.com/dapphub/hevm/compare/0.11.4...0.11.5
[0.11.4]: https://github.com/dapphub/hevm/compare/0.11.3...0.11.4
[0.11.3]: https://github.com/dapphub/hevm/compare/0.11.2...0.11.3
[0.11.2]: https://github.com/dapphub/hevm/compare/0.11.1...0.11.2
[0.11.1]: https://github.com/dapphub/hevm/compare/0.11...0.11.1
[0.11]: https://github.com/dapphub/hevm/compare/0.10.9...0.11
[0.10.9]: https://github.com/dapphub/hevm/compare/0.10.7...0.10.9
[0.10.7]: https://github.com/dapphub/hevm/compare/0.10.6...0.10.7
[0.10.6]: https://github.com/dapphub/hevm/compare/0.10.5...0.10.6
[0.10.5]: https://github.com/dapphub/hevm/compare/0.10...0.10.5
[0.10]: https://github.com/dapphub/hevm/compare/0.9.5...0.10
[0.9.5]: https://github.com/dapphub/hevm/compare/0.9...0.9.5
[0.9]: https://github.com/dapphub/hevm/compare/v0.8.5...v0.9
[0.8.5]: https://github.com/dapphub/hevm/compare/v0.8...v0.8.5
[0.8]: https://github.com/dapphub/hevm/compare/v0.7...v0.8
[0.7]: https://github.com/dapphub/hevm/compare/v0.6.5...v0.7
[0.6.5]: https://github.com/dapphub/hevm/compare/v0.6.1...v0.6.5
[0.6.1]: https://github.com/dapphub/hevm/compare/v0.6...v0.6.1
[0.6]: https://github.com/dapphub/hevm/compare/v0.5...v0.6
[0.5]: https://github.com/dapphub/hevm/compare/v0.4...v0.5
[0.4]: https://github.com/dapphub/hevm/compare/v0.3.2...v0.4
[0.3.2]: https://github.com/dapphub/hevm/compare/v0.3.0...v0.3.2
[0.3.0]: https://github.com/dapphub/hevm/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/dapphub/hevm/compare/v0.1.0.1...v0.2.0
[0.1.0.1]: https://github.com/dapphub/hevm/compare/v0.1.0.0...v0.1.0.1
