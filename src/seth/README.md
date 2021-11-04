# Seth

Seth is an Ethereum client tool—like a "MetaMask for the command
line"—maintained by [the DappHub collective].

:older_woman: If you're a **command-line hacker**, Seth will make you
go "It's a Unix system—I know this!"

:fax: If you're doing **blockchain automation**, Seth is an excellent
base for deploy scripts, integration tests, and bots.

:money_with_wings: If you love **open source finance**, Seth is a
sci-fi future where you can manage funds from the command line.

seth supports signing transactions with [Ledger Nano S] or Trezor
hardware wallets—even if you use a remote RPC node like Infura's.

> "One indicator I look for in a healthy open source project is how
> many useful tools come out of its team as a side effect of their
> efforts."
> —[`@danfinlay`](https://twitter.com/danfinlay/status/942909341044162560)
>
> "Looks like a great set of CLI tools, very devopsy." —Andreas Antonopolous
>
> "The Unix approach you've taken is perfect." —`immutability`

<div align="center">
  <h3>
    <a href=https://dapphub.chat>dapphub.chat</a>
    <span> — </span>
    <a href=https://dapp.tools>dapp.tools</a>
    <span> — </span>
    <a href=https://dapphub.com>dapphub.com</a>
  </h3>
</div>
<br />

## Contents

- [Installing](#installing)
- [Configuration](#configuration)
  - [Example `.sethrc` file](#example-sethrc-file)
  - [Connecting to the blockchain](#connecting-to-the-blockchain)
  - [Key management and signing](#key-management-and-signing)
  - [Hardware wallets](#hardware-wallets)
  - [Your address](#your-address)
- [Basic usage: a tutorial](#basic-usage-a-tutorial)
  - [Ether transactions](#ether-transactions)
  - [Helper commands](#helper-commands)
  - [Checking ether balances](#checking-ether-balances)
  - [Reading from contracts](#reading-from-contracts)
  - [Transacting with contracts](#transacting-with-contracts)
  - [Using Strings](#using-strings)
  - [Using Arrays](#using-arrays)
- [Commands](#commands)
  - [`seth --abi-decode`]
  - [`seth --calldata-decode`]
  - [`seth --from-ascii`]
  - [`seth --from-bin`]
  - [`seth --from-fix`]
  - [`seth --from-wei`]
  - [`seth --max-int`]
  - [`seth --max-uint`]
  - [`seth --min-int`]
  - [`seth --to-address`]
  - [`seth --to-ascii`]
  - [`seth --to-bytes32`]
  - [`seth --to-dec`]
  - [`seth --to-fix`]
  - [`seth --to-hex`]
  - [`seth --to-int256`]
  - [`seth --to-uint256`]
  - [`seth --to-wei`]
  - [`seth 4byte`]
  - [`seth 4byte-decode`]
  - [`seth 4byte-event`]
  - [`seth abi-encode`]
  - [`seth age`]
  - [`seth balance`]
  - [`seth basefee`]
  - [`seth block`]
  - [`seth block-number`]
  - [`seth bundle-source`]
  - [`seth call`]
  - [`seth calldata`]
  - [`seth chain`]
  - [`seth chain-id`]
  - [`seth code`]
  - [`seth debug`]
  - [`seth estimate`]
  - [`seth etherscan-source`]
  - [`seth events`]
  - [`seth gas-price`]
  - [`seth index`]
  - [`seth keccak`]
  - [`seth logs`]
  - [`seth lookup-address`]
  - [`seth ls`]
  - [`seth mktx`]
  - [`seth namehash`]
  - [`seth nonce`]
  - [`seth publish`]
  - [`seth receipt`]
  - [`seth resolve-name`]
  - [`seth run-tx`]
  - [`seth send`]
  - [`seth sign`]
  - [`seth storage`]
  - [`seth tx`]

<br />

## Installing

Seth is distributed as part of the [Dapp
tools](https://github.com/dapphub/dapptools) suite.

## Configuration

Seth has options that can be specified via command-line flags
or environment variables.

Seth looks in the following places for configuration, in descending order of precedence:

- `./.sethrc`
- `XDG_CONFIG_HOME/seth/sethrc`
- `~/.sethrc`

The configuration is loaded as a shell script (Bash 4 syntax). This is a convenient place to set default
options by exporting environment variables.

### Example `.sethrc` file

    # Use Infura's mainnet node for all RPC calls
    export SETH_CHAIN=ethlive

    # Set an address as the default sender
    export ETH_FROM=0xd08f67044c53d723686e002c5b880f73674e164c

    # Look for my key files in a custom directory
    export ETH_KEYSTORE=~/secrets/ethereum

### Example `.sethrc` file that uses infura Kovan testnet

    # Use Infura's Kovan testnet node for all RPC calls
    export ETH_RPC_URL=https://kovan.infura.io/v3/<API-KEY>

    # Set an address as the default sender
    export ETH_FROM=0xd08f67044c53d723686e002c5b880f73674e164c

Note that flags given to the `seth` command will override
these settings.

### Connecting to the blockchain

By default, Seth assumes a local RPC node on the standard port.

You can specify another RPC URL using the variable `ETH_RPC_URL`
or the flag `--rpc-url`.

Alternatively, you can use a default remote node (operated by
[Infura]) using the variable `SETH_CHAIN` or the flag `--chain` (or
`-C`). Allowed values: `ethlive` (aka `mainnet`), `ropsten`, `kovan`, `rinkeby`, and `goerli`.

Note: If you frequently use Seth with the Infura nodes, you should
[sign up for an Infura API key](https://infura.io/register) and use your
custom URL:

    export ETH_RPC_URL=https://mainnet.infura.io/<API-KEY>

### Key management and signing

By default, Seth does not use the RPC node for key management or
signing transactions. Instead, it uses keys stored on your machine,
as well as your Ledger Nano S hardware wallet (if present). **Thus,
you do not need to "unlock" your account in Geth or Parity.**

Seth looks for keys in the standard directories of Geth and Parity.
To configure a custom location for your key files, use the
`ETH_KEYSTORE` variable or the `--keystore` flag. The
`ETH_KEYSTORE` variable should point to a directory containing [JSON keystore wallet files](https://theethereum.wiki/w/index.php/Accounts,_Addresses,_Public_And_Private_Keys,_And_Tokens).
If you would like to use an existing private key that you do not have a keystore file for,
you may use [`ethsign import`](https://github.com/dapphub/dapptools/tree/master/src/ethsign).

Use `seth accounts` or `seth ls` to list out all wallets in the directory.

If your key is protected with a password, Seth will prompt you each
time you make a transaction. If you are confident in your computer
security, and you want to (say) run a bot script, you can set the
`ETH_PASSWORD` variable (flag: `--password`) to point to a file
containing your password.

If you do want to use the RPC node for key management and signing, set
the `ETH_RPC_ACCOUNTS` variable or use the `--rpc-accounts` flag.
This probably means you need to use Geth's or Parity's account
management tools to "unlock" your account.

If both `ETH_RPC_ACCOUNTS` and `ETH_FROM` are set, `seth` will first check if the
provided account can be found in the keystore, or in any connected hardware
wallet, and only use the RPC node's signer if the account does not exist.
Note: Seth uses the [`ethsign`] tool for signing transactions.
This tool uses Geth as a library.

### Hardware wallets

Seth will scan for [Ledger Nano S] hardware wallets by default.

The Ledger wallet is only available to Seth while it is unlocked, in
the Ethereum app, and with **browser mode off**.

On Linux, you may have to enable some USB permissions. See the
[Ledger Wallet Linux instructions].

When you use a sending address that belongs to the hardware wallet,
Seth will automatically use it for signing transactions.

**Note:** Seth currently only looks for the first four addresses
derived from your seed phrase. You may customize the hd derivation path
by setting the `ETH_HDPATH` variable.

### Your address

When making transactions or doing read-only calls, Seth takes the
sending address from the `ETH_FROM` variable or the `--from` flag.

<br />

## Basic usage: a tutorial

This section assumes that you have something like the
[example `.sethrc` file](#example-sethrc-file) specifying how to
connect to the blockchain and a default sender address.

### Ether transactions

Here is how you might use [`seth send`](#seth-send) to send one
wei—the smallest possible amount of ether—to the [Ethereum
Foundation's donation address]:

    $ seth send --value 1 0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    seth-send: warning: `ETH_GAS' not set; using default gas amount
    Ethereum account passphrase (not echoed):
    seth-send: Published transaction with 0 bytes of calldata.
    seth-send: 0xe428d4bb148ded426777ae892578507e4f394f608ad9d3a9d0229e8348ba72e3
    seth-send: Waiting for transaction receipt...
    seth-send: Transaction included in block 4908738.

### Helper commands

The `$(...)` shell syntax for ["command substitution"] is very useful
with Seth. It allows the output of one command to become a parameter
to another. An example follows.

Generally, you don't transact in terms of wei amounts, but in
fractional amounts of ether. You can convert an ether amount into a
wei amount using [`seth --to-wei`]. Here, we send 1.5
ETH:

    $ fund=0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    $ seth send --value $(seth --to-wei 1.5 eth) $fund

There is also [`seth --from-wei`] for converting wei amounts into a
more readable notation.

For more advanced blockchain interactions, the helpers [`seth
--abi-decode`], [`seth --from-ascii`], and [`seth --from-bin`] are
also useful.

### Checking ether balances

You can use [`seth balance`] to see how much is in the donation
fund:

    $ seth balance 0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    4595456374254502385669

You can use [`seth ls`] to check the ether balances of your own
accounts:

    $ seth ls
    0xCC41D9831E4857B4F16914A356306fBeA734183A    2412312951251268
    0xD9ceccea2BEE9a367d78658aBbB2Fe979b3877Ef    142109

### Reading from contracts

The basic tool to read information from a contract is [`seth call`],
which performs a call without publishing a transaction.

For example, you can read the total supply of the [MakerDAO]
governance token using the ERC20 ABI:

    $ MKR_TOKEN=0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2
    $ seth call $MKR_TOKEN "totalSupply()(uint)"
    995238778286468792512963

If the ABI function has parameters, you can supply them as additional
arguments; for example, to check the balance of the MakerDAO fund:

    $ MKR_FUND=0x7Bb0b08587b8a6B8945e09F1Baca426558B0f06a
    $ seth call $MKR_TOKEN "balanceOf(address)(uint)" $MKR_FUND
    0

(See also [`token`](https://github.com/dapphub/dapptools/tree/master/src/token) for a more convenient way to use ERC20 tokens.)

You can also use [`seth logs`] to read event logs from a contract or
[`seth code`] to get a contract's bytecode.

### Transacting with contracts

The [`seth send`] tool is not only capable of sending ether, but also
of constructing, signing, and publishing contract transactions.
This requires that you know the exact ABI method to use.

For example, to approve access to some of your [Dai] for the
[OasisDEX] decentralized exchange using the ERC20 approval interface:

    $ DAI=0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359
    $ OASIS=0x14FBCA95be7e99C15Cc2996c6C9d841e54B79425
    $ amount=$(seth --to-wei 0.5 ether)
    $ seth send $DAI "approve(address,uint256)" $OASIS $amount

(Again, see [`token`](https://github.com/dapphub/dapptools/tree/master/src/token) for a more convenient way to interact with
ERC20 tokens.)

See [`seth send`] for details on passing arguments, doing asynchronous
transactions, exit codes, and so on.

### Using strings

Strings can be used by enclosing them in double quotes within single quotes.

    $ export RESULT=$(seth calldata "f(string)" '"Hello World"')
    $ seth --calldata-decode "f(string)" $RESULT
    Hello World

### Using arrays

Arrays can be used by enclosing them in single or double quotes. Arrays
surrounded in single quotes will be inerpreted literally - you won't be able
to use variables (`$FOO`) in them.

    $ export AMOUNT=$(seth --to-wei 5 ether)
    $ export TEST=$(seth calldata "f(uint256[])" "[$AMOUNT]"
    $ seth --calldata-decode "f(uint256[])" $TEST
    5000000000000000000

<br />

# Commands

### `seth --abi-decode`

Extract return values from hex data.

    seth --abi-decode "<name>(<in-types>)(<out-types>)" <hexdata>

Decodes `<hexdata>` according to `<out-types>` (`<in-types>` are ignored).

### `seth --calldata-decode`

Decodes a calldata bytestring into a list of input arguments.

    seth --calldata-decode <signature> <hexstring>

### `seth --from-ascii`

Convert text data into hex data.

    seth --from-ascii <text>...

### `seth --from-bin`

Convert binary data into hex data.

    seth --from-bin <data.bin >data.hex

Reads binary data from standard input and prints it as hex data.

### `seth --from-fix`

Convert fixed point numbers into parsed integers with the specified number of decimals.

    seth --from-fix <decimals> <value>

For example, use `seth --to-fix 6 1` to convert 1 USDC into the parsed quantity of 1,000,000 USDC

### `seth --from-wei`

Convert a wei amount into another unit (wei by default).

    seth --from-wei <value> [<unit>]

The unit may be `wei`, `gwei`, `eth`, or `ether`.

### `seth --max-int`

Returns the max signed integer with the specified number of bits.

    seth --max-int [<bits>]

Defaults to 256 bits.

### `seth --max-uint`

Returns the max unsigned integer with the specified number of bits.

    seth --max-uint [<bits>]

Defaults to 256 bits.

### `seth --min-int`

Returns the min signed integer with the specified number of bits.

    seth --max-uint [<bits>]

Defaults to 256 bits.

### `seth --to-address`

Convert an address into a checksummed address.

    seth --to-address <address>

### `seth --to-ascii`

Convert hex data into text data.

    seth --to-ascii <hexdata>

### `seth --to-bytes32`

Pad a hex string to the right with zeroes to 32 bytes.

    seth --to-bytes32 <value>

### `seth --to-dec`

Convert a hex value with 0x prefix into a decimal number.

    seth --to-dec <hexvalue>

### `seth --to-fix`

Convert parsed integers into fixed point with the specified number of decimals.

    seth --to-fix <decimals> <value>

For example, use `seth --to-fix 6 1000000` to convert the parsed amount of 1,000,000 USDC into a formatted amount of 1 USDC.

### `seth --to-hex`

Convert a decimal number into a hex value.

    seth --to-hex <value>

### `seth --to-int256`

Convert a number into int256 hex string with 0x prefix.

    seth --to-int256 <value>

### `seth --to-uint256`

Convert a number into uint256 hex string with 0x prefix.

    seth --to-uint256 <value>

### `seth --to-wei`

Convert an ETH amount into wei.

    seth --to-wei <value> [<unit>]

The unit may be `wei`, `gwei`, `eth`, or `ether`.

### `seth 4byte`

Prints the response from querying [4byte.directory](https://www.4byte.directory/) for a given function signature

    seth 4byte <calldata> [<options>]

Any calldata appended after the function signature will be stripped before querying 4byte.directory.

By default, just the signatures will be printed, but the `-v` flag can be used to print the full JSON response.

### `seth 4byte-decode`

Queries [4byte.directory](https://www.4byte.directory/) for matching function signatures, uses one to decode the calldata, and prints the decoded calldata.

    seth 4byte-decode <calldata> [<options>]

By default, the user will be prompted to select a function signature to use for decoding the calldata.

The `--id` flag can be passed to bypass interactive mode.
Use `--id earliest` or `--id latest` to use the oldest and newest functions in the 4byte.directory database, respectively.
Use `--id <number>` to select a function signature by it's ID in the 4byte.directory database.

### `seth 4byte-event`

Prints the response from querying [4byte.directory](https://www.4byte.directory/) for a given event topic

    seth 4byte-event <topic> [<options>]

By default, just the signatures will be printed, but the `-v` flag can be used to print the full JSON response.

### `seth abi-encode`

Prints the ABI encoded values without the function signature

    seth abi-encode <sig> [<args>]

ABI encode values based on a provided function signature, slice off the leading the function signature,
and print the result. It does not matter what the name of the function is, as only the types and values
affect the output.

### `seth age`

Show the timestamp of a block (the latest block by default).

    seth age [--block <block>]

### `seth balance`

Show the ether balance of an account.

    seth balance [--block <block>] <account>

### `seth basefee`

Show the basefee of a block (the latest block by default).

    seth basefee [<block>]

If no `<block>` number is given, defaults to `latest`.

### `seth block`

Print a table of information about a specific block.

    seth block [--json] <block> [<field>]

If `<field>` is given, print only the value of that field.

The `<block>` may be either `latest`, a block hash, or a block number.

### `seth block-number`

Returns the latest block number.

    seth block-number

### `seth bundle-source`

Fetch a contract source from etherscan and compile it
with the appropriate Solidity version. Useful to
provide source maps in calls to [`seth run-tx`] or [`hevm exec --debug --rpc`](../hevm/README.md#hevm-exec).

    seth bundle-source <address>

Requires the `ETHERSCAN_API_KEY` environment variable to be set.

Use `--dir` to control the directory in which compilation occurs (defaults to current working directory)

### `seth call`

Call a contract without updating the blockchain.

    seth call [<options>] <to> <sig> [<args>...]
    seth call [<options>] <to> [<calldata>]
    seth call [<options>] --create <code> <sig> [<args>]
    seth call [<options>] --create <code> [<data>]

When given `<sig>` of the form `<name>(<types>)`, perform ABI encoding
to infer the calldata.

When `<sig>` also includes a return type, as
`name(<in-types>)(<out-types>)`, then also decode the return value.

Otherwise `<calldata>` should be hex data.

When `--create` is passed, read the `<code>` to use in a creation call,
optionally passing additional hex `<data>` or structured `<sig>` and
`<args>`.

| Flag      | Variable    | Default  | Synopsis               |
| --------- | ----------- | -------- | ---------------------- |
| `--block` | `ETH_BLOCK` | `latest` | block number           |
| `--from`  | `ETH_FROM`  | n/a      | simulated sender       |
| `--gas`   | `ETH_GAS`   | `200000` | simulated gas quantity |
| `--value` | `ETH_VALUE` | `0`      | simulated ether value  |

By default, calls are made to the defined RPC node. With `--hevm`, calls
are evaluated locally with [`hevm`].

If `--debug` is passed, the call will be displayed interactively in the
`hevm` debugger.

If `--code <code>` is passed, the `<to>` address will have its runtime
bytecode overwritten with `<code>`, against which the call will then be
evaluated.

### `seth calldata`

Pack a signature and an argument list into hexadecimal calldata.

    seth calldata <sig> [<args>...]
    seth calldata <file>
    seth calldata <data>

When called with `<sig>` of the form `<name>(<types>...)`, then
perform ABI encoding to produce the hexadecimal calldata.

If `<file>` is given—containing at least one slash character—then
treat it as a file name to read, and proceed as if the contents were
passed as `<data>`.

Given `<data>`, ensure it is hexadecimal calldata starting with `0x`
and normalize it to lowercase.

### `seth chain`

Print the symbolic name of the current blockchain by checking the
genesis block hash.

Outputs one of `ethlive`, `etclive`, `kovan`, `ropsten`, `goerli`, `morden`,
`rinkeby`, `optimism-mainnet`, `optimism-kovan`, `arbitrum-mainnet`, `bsc`, `bsctest`, `kotti`, `polygon`, or `unknown`.

### `seth chain-id`

Print the ethereum chain id. `1` for Mainnet, `42` for Kovan, etc.

### `seth code`

Print the bytecode of a contract.

    seth code [--block <block>] <address>

If `<block>` is not given, the default is `latest`.

### `seth debug`

Step through a transaction in the interactive debugger.
Executes all prior transactions in the block to ensure correct
state. This may take a while.
If you are in a hurry or don't expect the relevant state to be changed
by other transactions in the block, use [`seth run-tx`] instead.

    seth debug <txhash> [<options>]

Unless `--no-src` is given, seth will try to fetch the source code for
the target of the transaction for better debugging xp.

### `seth estimate`

Estimate how much gas a transaction is likely to use, using the RPC
node's gas estimation.

    seth estimate [<options>] <to> <sig> [<args>]
    seth estimate [<options>] <to> <sig> [<args>]
    seth estimate [<options>] --create <code> <sig> [<args>]
    seth estimate [<options>] --create <code> <data>

Options are similar to [`seth send`], but no transaction is published.

### `seth etherscan-source`

Fetch the source of a contract from etherscan. Requires etherscan api key.

    seth etherscan-source <address> [<options>

Returns a json with source and options. For just the source, try:

`seth etherscan-source <address> | jq .SourceCode -r`

### `seth events`

Print the decoded events of a contract.

    seth events [--block <block>] [--follow] <address>

To use this command, you need to set the `SETH_ABI` variable:

    export SETH_ABI=$(seth abi "event Foo(uint bar);")

To use a JSON ABI file:

    export SETH_ABI=$(seth --decorate-abi $(cat abi.json))

With `--follow`, the command blocks waiting for new events (like `tail -f`).

See also [`seth logs`] which does not decode events.

### `seth gas-price`

Reads the current gas price at target chain.

### `seth index`

Prints the slot number for the specified mapping type and input data

    seth index <fromtype> <totype> <fromvalue> <slot> [<lang>]

`lang` will default to Solidity when not specified.
To compute the slot for Vyper instead, specify `v`, `vy`, or `vyper`.

Result is not guaranteed to be accurate for all Vyper versions since the Vyper storage layout is not yet stable.

### `seth keccak`

Print the Keccak-256 hash of an arbitrary piece of data.

    seth keccak <data>

Note: this uses the RPC node for hashing, which may be inefficient.

### `seth logs`

Print the undecoded transaction logs of a contract.

    seth logs [--block <block>] [--follow] <address>

With `--follow`, the command blocks waiting for new events
(like `tail -f`).

See also [`seth events`] which decodes logs using an
ABI specification.

### `seth lookup-address`

Print the ENS name the provided address reverse resolves to. If the name is
not owned or does not have a resolver configured, an `invalid data for
function output` error will be thrown. An error will also be thrown
if the forward and reverse resolution do not match.

    seth lookup-address <address>

### `seth ls`

Display a list of your accounts and their ether balances.

See [Key management and signing](#key-management-and-signing) for
details on how Seth finds your accounts.

### `seth mktx`

Make and signs a transaction without publishing it.

    seth mktx [<options>] <to> <sig> [<args>]
    seth mktx [<options>] <to> <calldata>

Options are as for [`seth send`] but no transaction is published.

See also [`seth publish`] for publishing a signed transaction.

### `seth namehash`

Print the ENS namehash of the provided name.

    seth namehash <name>

ENS names are converted to lowercase before hashing, but note this is
not the complete [normalization process](https://docs.ens.domains/contract-api-reference/name-processing#normalising-names),
so users must ensure the ENS names they enter are properly formatted.

### `seth nonce`

Show the number of transactions successfully sent from an address (its
nonce).

    seth nonce [--block <block>] <address>

### `seth publish`

Publish an already signed transaction to the blockchain.

    seth publish [<txdata>]

If `<txdata>` is not given, read it from standard input instead.

### `seth receipt`

Wait for a transaction receipt to appear and print it in tabular form.

    seth receipt [--async] <txhash> [<field>]

Print all fields of the transaction receipt unless `<field>`
is specified.

Unless `--async` is given, wait indefinitely for the receipt
to appear.

### `seth resolve-name`

Print the address the provided ENS name resolves to. If the name is not
owned or does not have a resolver configured, an `invalid data for function output`
error will be thrown.

    seth resolve-name <name>

ENS names are converted to lowercase before hashing, but note this is
not the complete [normalization process](https://docs.ens.domains/contract-api-reference/name-processing#normalising-names),
so users must ensure the ENS names they enter are properly formatted.

### `seth run-tx`

Run a transaction with hevm in the environment of the given transaction.

    seth run-tx <tx-hash> [<options>]

Attempts to fetch contract source from etherscan if `ETHERSCAN_API_KEY` is set.

With `--state dir`, load and save state from `dir`
With `--trace`, print the call trace of the transaction.
With `--debug`, execute with hevm's interactive debugger
With `--no-src`, do not attempt to fetch contract source from etherscan
With `--source=<filename>`, manually supply a solc compiler output json (implies `--no-src`)

### `seth send`

Sign and publish a transaction to the blockchain.

    seth send [<options>] <to> <sig> [<args>]
    seth send [<options>] <to> [<data>]
    seth send [<options>] --create <code> <sig> [<args>]
    seth send [<options>] --create <code> [<data>]

| Flag          | Variable        | Default      | Synopsis                          |
| ------------- | --------------- | ------------ | ---------------                   |
| `--block`     | `ETH_BLOCK`     | `latest`     | block number                      |
| `--from`      | `ETH_FROM`      | n/a          | sender                            |
| `--gas`       | `ETH_GAS`       | `200000`     | gas quantity                      |
| `--gas-price` | `ETH_GAS_PRICE` |              | gas price                         |
| `--prio-fee`  | `ETH_PRIO_FEE`  |              | EIP-1559 priority fee (miner tip) |
| `--value`     | `ETH_VALUE`     | `0`          | ether value                       |
| `--create`    | `SETH_CREATE`   |              | create contract                   |
| `--resend`    | `SETH_RESEND`   |              | reuse nonce                       |
| `--async`     | `SETH_ASYNC`    |              | don't wait                        |
| `--status`    | `SETH_STATUS`   |              | check success                     |


See [Key management and signing](#key-management-and-signing) for
details on how Seth signs transactions.

With `--async`, just print the transaction hash.
Otherwise, wait for the receipt and print as with [`seth receipt`].

With `--status` (which excludes `--async`), check the status field of
the transaction receipt and exit with an error code if the transaction
failed. This is a post-Byzantium feature and will soon become the
default behavior.

If `--gas-price` is provided (or `ETH_GAS_PRICE`) is set, legacy transactions will be used.
For dynamic fee transactions (EIP-1559), `--prio-fee` is required.

### `seth sign`

    seth sign <data>

Sign hexdata with the `'\x19Ethereum Signed Message:\n'` prefix using the `$ETH_FROM`
account.

See [`ethsign`](../ethsign/README.md) for more signing and key management options.

### `seth storage`

Show the raw value of a contract's storage slot.

    seth storage [--block <block>] <address> <slot>

### `seth tx`

Print a table of information about a transaction.

    seth tx <txhash> [<field>]

Show all fields unless `<field>` is given.

[the dapphub collective]: https://dapphub.com
[ledger nano s]: https://www.ledgerwallet.com/products/ledger-nano-s
[infura]: https://infura.io
["command substitution"]: https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html
[ethereum foundation's donation address]: https://www.ethereum.org/donate
[the nix package manager]: https://nixos.org/nix
[makerdao cdp utility]: https://github.com/makerdao/dai-cli
[`seth --abi-decode`]: #seth---abi-decode
[`seth --from-ascii`]: #seth---from-ascii
[`seth --from-bin`]: #seth---from-bin
[`seth --from-fix`]: #seth---from-fix
[`seth --from-wei`]: #seth---from-wei
[`seth --to-fix`]: #seth---to-fix
[`seth --to-wei`]: #seth---to-wei
[`seth --to-int256`]: #seth---to-int256
[`seth --to-uint256`]: #seth---to-uint256
[`seth --to-bytes32`]: #seth---to-bytes32
[`seth --to-address`]: #seth---to-address
[`seth --max-int`]: #seth---max-int
[`seth --max-uint`]: #seth---max-uint
[`seth --min-int`]: #seth---min-int
[`seth --to-ascii`]: #seth---to-ascii
[`seth --to-dec`]: #seth---to-dec
[`seth --to-hex`]: #seth---to-hex
[`seth --calldata-decode`]: #seth---calldata-decode
[`seth block-number`]: #seth-block-number
[`seth gas-price`]: #seth-gas-price
[`seth 4byte`]: #seth-4byte
[`seth 4byte-decode`]: #seth-4byte-decode
[`seth 4byte-event`]: #seth-4byte-event
[`seth abi-encode`]: #seth-abi-encode
[`seth abi`]: #seth-abi
[`seth age`]: #seth-age
[`seth balance`]: #seth-balance
[`seth basefee`]: #seth-basefee
[`seth block`]: #seth-block
[`seth bundle-source`]: #seth-bundle-source
[`seth call`]: #seth-call
[`seth calldata`]: #seth-calldata
[`seth chain`]: #seth-chain
[`seth chain-id`]: #seth-chain-id
[`seth code`]: #seth-code
[`seth debug`]: #seth-debug
[`seth estimate`]: #seth-estimate
[`seth etherscan-source`]: #seth-etherscan-source
[`seth events`]: #seth-events
[`seth help`]: #seth-help
[`seth index`]: #seth-index
[`seth keccak`]: #seth-keccak
[`seth logs`]: #seth-logs
[`seth lookup-address`]: #seth-lookup-address
[`seth ls`]: #seth-ls
[`seth mktx`]: #seth-mktx
[`seth namehash`]: #seth-namehash
[`seth nonce`]: #seth-nonce
[`seth publish`]: #seth-publish
[`seth receipt`]: #seth-receipt
[`seth resolve-name`]: #seth-resolve-name
[`seth run-tx`]: #seth-run-tx
[`seth send`]: #seth-send
[`seth sign`]: #seth-sign
[`seth storage`]: #seth-storage
[`seth tx`]: #seth-tx
[makerdao]: https://makerdao.com
[dai stablecoin system]: https://makerdao.com
[dai]: https://makerdao.com
[oasisdex]: https://oasisdex.com
[`ethsign`]: https://github.com/dapphub/dapptools/blob/master/src/ethsign/README.md
[`hevm`]: https://github.com/dapphub/dapptools/blob/master/src/hevm/README.md
[ledger wallet linux instructions]: https://ledger.zendesk.com/hc/en-us/articles/115005165269-What-if-Ledger-Wallet-is-not-recognized-on-Linux-
