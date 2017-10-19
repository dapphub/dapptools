# hevm changelog

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
