Seth
========================================================================

Seth is an Ethereum client tool—like a "MetaMask for the command
line"—maintained by [the DappHub collective].

:older_woman: If you're a **command-line hacker**, Seth will make you
go *"It's a Unix system—I know this!"*

:fax: If you're doing **blockchain automation**, Seth is an excellent
base for deploy scripts, integration tests, and bots.

:money_with_wings: If you love **open source finance**, Seth is a
sci-fi future where you can manage funds from the command line.

**New:** Seth supports signing transactions with [Ledger Nano S]
hardware wallets—even if you use a remote RPC node like Infura's.

> "One indicator I look for in a healthy open source project is how
  many useful tools come out of its team as a side effect of their
  efforts."
  —[`@danfinlay`](https://twitter.com/danfinlay/status/942909341044162560)
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

Contents
------------------------------------------------------------------------

  * [Installing](#installing)
      * [Upgrading](#upgrading)
  * [Configuration](#configuration)
      * [Example `.sethrc` file](#example-sethrc-file)
      * [Connecting to the blockchain](#connecting-to-the-blockchain)
      * [Key management and signing](#key-management-and-signing)
      * [Hardware wallets](#hardware-wallets)
      * [Your address](#your-address)
  * [Basic usage: a tutorial](#basic-usage-a-tutorial)
      * [Ether transactions](#ether-transactions)
      * [Helper commands](#helper-commands)
      * [Checking ether balances](#checking-ether-balances)
      * [Reading from contracts](#reading-from-contracts)
      * [Transacting with contracts](#transacting-with-contracts)
  * [Commands](#commands)
      * [`seth --abi-decode`]
      * [`seth --from-ascii`]
      * [`seth --from-bin`]
      * [`seth --from-wei`]
      * [`seth --to-wei`]
      * [`seth --to-int256`]
      * [`seth --to-uint256`]
      * [`seth --to-bytes32`]
      * [`seth --to-address`]
      * [`seth gas-price`]
      * [`seth age`]
      * [`seth balance`]
      * [`seth block`]
      * [`seth call`]
      * [`seth calldata`]
      * [`seth chain`]
      * [`seth code`]
      * [`seth estimate`]
      * [`seth events`]
      * [`seth help`]
      * [`seth keccak`]
      * [`seth logs`]
      * [`seth ls`]
      * [`seth mktx`]
      * [`seth nonce`]
      * [`seth publish`]
      * [`seth receipt`]
      * [`seth send`]
      * [`seth sign`]
      * [`seth storage`]
      * [`seth tx`]

<br />

Installing
------------------------------------------------------------------------

Seth is distributed as part of the [Dapp
tools](https://github.com/dapphub/dapptools) suite.

Configuration
------------------------------------------------------------------------

Seth has options that can be specified via command-line flags
or environment variables.

Seth looks in the following places for configuration, in descending order of precedence:

* `./.sethrc`
* `XDG_CONFIG_HOME/seth/sethrc`
* `~/.sethrc`

The configuration is loaded as a shell script (Bash 4 syntax).  This is a convenient place to set default
options by exporting environment variables.

### Example `.sethrc` file

    # Use Infura's mainnet node for all RPC calls
    export SETH_CHAIN=ethlive

    # Set an address as the default sender
    export ETH_FROM=0xd08f67044c53d723686e002c5b880f73674e164c

    # Look for my key files in a custom directory
    export ETH_KEYSTORE=~/secrets/ethereum

Note that flags given to the `seth` command will override
these settings.

### Connecting to the blockchain

By default, Seth assumes a local RPC node on the standard port.

You can specify another RPC URL using the variable `ETH_RPC_URL`
or the flag `--rpc-url`.

Alternatively, you can use a default remote node (operated by
[Infura]) using the variable `SETH_CHAIN` or the flag `--chain` (or
`-C`).  Allowed values: `ethlive` (aka `mainnet`), `ropsten`, `kovan`, `rinkeby`, and `goerli`.

Note: If you frequently use Seth with the Infura nodes, you should
[sign up for an Infura API key](https://infura.io/register) and use your
custom URL:

    export ETH_RPC_URL=https://mainnet.infura.io/<API-KEY>

### Key management and signing

By default, Seth does not use the RPC node for key management or
signing transactions.  Instead, it uses keys stored on your machine,
as well as your Ledger Nano S hardware wallet (if present).  **Thus,
you do not need to "unlock" your account in Geth or Parity.**

Seth looks for keys in the standard directories of Geth and Parity.
To configure a custom location for your key files, use the
`ETH_KEYSTORE` variable or the `--keystore` flag. The
`ETH_KEYSTORE` variable should point to a directory containing [JSON keystore wallet files](https://theethereum.wiki/w/index.php/Accounts,_Addresses,_Public_And_Private_Keys,_And_Tokens). Use `seth accounts` or `seth ls` to list out all wallets in the directory.

If your key is protected with a password, Seth will prompt you each
time you make a transaction.  If you are confident in your computer
security, and you want to (say) run a bot script, you can set the
`ETH_PASSWORD` variable (flag: `--password`) to point to a file
containing your password.

If you do want to use the RPC node for key management and signing, set
the `ETH_RPC_ACCOUNTS` variable or use the `--rpc-accounts` flag.
This probably means you need to use Geth's or Parity's account
management tools to "unlock" your account.

Note: Seth uses the [`ethsign`] tool for signing transactions.
This tool uses Geth as a library.

### Hardware wallets

Seth will scan for [Ledger Nano S] hardware wallets by default.

The Ledger wallet is only available to Seth while it is unlocked, in
the Ethereum app, and with **browser mode off**.

On Linux, you may have to enable some USB permissions.  See the
[Ledger Wallet Linux instructions].

When you use a sending address that belongs to the hardware wallet,
Seth will automatically use it for signing transactions.

**Note:** Seth currently only looks for the first four addresses
derived from your seed phrase.  If the sending address is not one of
those, Seth will not be able to sign transactions.

### Your address

When making transactions or doing read-only calls, Seth takes the
sending address from the `ETH_FROM` variable or the `--from` flag.

<br />

Basic usage: a tutorial
------------------------------------------------------------------------

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
with Seth.  It allows the output of one command to become a parameter
to another.  An example follows.

Generally, you don't transact in terms of wei amounts, but in
fractional amounts of ether.  You can convert an ether amount into a
wei amount using [`seth --to-wei`].  Here, we send 1.5
ETH:

    $ fund=0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    $ seth send --value $(seth --to-wei 1.5 eth) $fund

There is also [`seth --from-wei`] for converting wei amounts into a
more readable notation.

For more advanced blockchain interactions, the helpers [`seth
--abi-decode`], [`seth --from-ascii`], and [`seth --from-bin`] are
also useful.

### Checking ether balances

Now you can use [`seth balance`] to see how much is in the donation
fund:

    $ seth balance 0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    2963.72865500027557173E+18

You can use [`seth ls`] to check the ether balances of your own
accounts:

    $ seth ls
    0xCC41D9831E4857B4F16914A356306fBeA734183A    0.24E+18
    0xD9ceccea2BEE9a367d78658aBbB2Fe979b3877Ef    0.03409E+18

### Reading from contracts

The basic tool to read information from a contract is [`seth call`],
which performs a call without publishing a transaction.

For example, you can read the total supply of the [MakerDAO]
governance token using the ERC20 ABI:

    $ MKR_TOKEN=0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2
    $ seth call $MKR_TOKEN "totalSupply()"
    0x00000000000000000000000000000000000000000000d3c21bcecceda1000000

If the ABI function has parameters, you can supply them as additional
arguments; for example, to check the balance of the MakerDAO fund:

    $ MKR_FUND=0x7Bb0b08587b8a6B8945e09F1Baca426558B0f06a
    $ seth call $MKR_TOKEN "balanceOf(address)" $MKR_FUND
    0x0000000000000000000000000000000000000000000050d7e9ff54cf2725f61b

(See also [`token`] for a more convenient way to use ERC20 tokens.)

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

(Again, see [`token`] for a more convenient way to interact with
ERC20 tokens.)

See [`seth send`] for details on passing arguments, doing asynchronous
transactions, exit codes, and so on.

<br />

Commands
========================================================================

### `seth --abi-decode`

Extract return values from hex data.

    seth --abi-decode "<name>(<in-types>)(<out-types>)" <hexdata>

Decodes `<hexdata>` according to `<out-types>` (`<in-types>` are ignored).

### `seth --from-ascii`

Convert text data into hex data.

    seth --from-ascii <text>...

### `seth --from-bin`

Convert binary data into hex data.

    seth --from-bin <data.bin >data.hex

Reads binary data from standard input and prints it as hex data.

### `seth --from-wei`

Convert a wei amount into another unit (ETH by default).

    seth --from-wei <value> [<unit>]

The unit may be `wei`, `gwei`, `eth`, or `ether`.

### `seth --to-wei`

Convert an ETH amount into wei.

    seth --to-wei <value> [<unit>]

The unit may be `wei`, `gwei`, `eth`, or `ether`.

### `seth --to-int256`

Convert a number into int256 hex string with 0x prefix.

    seth --to-int256 <value>

### `seth --to-uint256`

Convert a number into uint256 hex string with 0x prefix.

    seth --to-uint256 <value>

### `seth --to-bytes32`

Pad a hex string to the right with zeroes to 32 bytes.

    seth --to-bytes32 <value>

### `seth --to-address

Convert an address into a checksummed address.

    seth --to-address <address>

### `seth gas-price`

Reads the current gas price at target chain.

### `seth age`

Show the timestamp of a block (the latest block by default).

    seth age [--block <block>]

### `seth balance`

Show the ether balance of an account.

    seth balance [--block <block>] <account>

### `seth block`

Print a table of information about a specific block.

    seth block [--json] <block> [<field>]

If `<field>` is given, print only the value of that field.

The `<block>` may be either a block hash or a block number.

### `seth call`

Call a contract without updating the blockchain.

    seth call [<options>] <to> <sig> [<args>...]
    seth call [<options>] <to> <calldata>

When given `<sig>` of the form `<name>(<types>)`, perform ABI encoding
to infer the calldata.

When `<sig>` also includes a return type, as
`name(<in-types>)(<out-types>)`, then also decode the return value.

Otherwise `<calldata>` should be hex data.

| Flag      | Variable    | Default  | Synopsis               |
| --------- | ----------- | -------- | ---------------------- |
| `--block` | `ETH_BLOCK` | `latest` | block number           |
| `--from`  | `ETH_FROM`  | n/a      | simulated sender       |
| `--gas`   | `ETH_GAS`   | n/a      | simulated gas quantity |
| `--value` | `ETH_VALUE` | `0`      | simulated ether value  |

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
`rinkeby`, or `unknown`.

### `seth code`

Print the bytecode of a contract.

    seth code [--block <block>] <address>

If `<block>` is not given, the default is `latest`.

### `seth estimate`

Estimate how much gas a transaction is likely to use, using the RPC
node's gas estimation.

    seth estimate [<options>] <to> <sig> [<args>]
    seth estimate [<options>] <to> <sig> [<args>]
    seth estimate [<options>] --create <code> <sig> [<args>]
    seth estimate [<options>] --create <code> <data>

Options are similar to [`seth send`], but no transaction is published.

### `seth events`

Print the decoded events of a contract.

    seth events [--block <block>] [--follow] <address>

To use this command, you need to set the `SETH_ABI` variable:

    export SETH_ABI=$(seth abi "event Foo(uint bar);")

To use a JSON ABI file:

    export SETH_ABI=$(seth --decorate-abi $(cat abi.json))

With `--follow`, the command blocks waiting for new events (like `tail
-f`).

See also [`seth logs`] which does not decode events.

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

### `seth send`

Sign and publish a transaction to the blockchain.

    seth send [<options>] <to> <sig> [<args>]
    seth send [<options>] <to> [<data>]
    seth send [<options>] --create <code> <sig> [<args>]
    seth send [<options>] --create <code> [<data>]

| Flag          | Variable        | Default      | Synopsis        |
| ------------- | --------------- | ------------ | --------------- |
| `--block`     | `ETH_BLOCK`     | `latest`     | block number    |
| `--from`      | `ETH_FROM`      | n/a          | sender          |
| `--gas`       | `ETH_GAS`       | node decides | gas quantity    |
| `--gas-price` | `ETH_GAS_PRICE` | node decides | gas price       |
| `--value`     | `ETH_VALUE`     | `0`          | ether value     |
| `--create`    | `SETH_CREATE`   |              | create contract |
| `--resend`    | `SETH_RESEND`   |              | reuse nonce     |
| `--async`     | `SETH_ASYNC`    |              | don't wait      |
| `--status`    | `SETH_STATUS`   |              | check success   |

See [Key management and signing](#key-management-and-signing) for
details on how Seth signs transactions.

With `--async`, just print the transaction hash.
Otherwise, wait for the receipt and print as with [`seth receipt`].

With `--status` (which excludes `--async`), check the status field of
the transaction receipt and exit with an error code if the transaction
failed.  This is a post-Byzantium feature and will soon become the
default behavior.

### `seth storage`

Show the raw value of a contract's storage slot.

    seth storage [--block <block>] <address> <slot>

### `seth tx`

Print a table of information about a transaction.

    seth tx <txhash> [<field>]

Show all fields unless `<field>` is given.




[the DappHub collective]: https://dapphub.com

[Ledger Nano S]: https://www.ledgerwallet.com/products/ledger-nano-s

[Infura]: https://infura.io

["command substitution"]: https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html

[Ethereum Foundation's donation address]: https://www.ethereum.org/donate

[the Nix package manager]: https://nixos.org/nix

[MakerDAO CDP utility]: https://github.com/makerdao/dai-cli

[`seth --abi-decode`]: #seth---abi-decode
[`seth --from-ascii`]: #seth---from-ascii
[`seth --from-bin`]: #seth---from-bin
[`seth --from-wei`]: #seth---from-wei
[`seth --to-wei`]: #seth---to-wei
[`seth --to-int256`]: #seth---to-int256
[`seth --to-uint256`]: #seth---to-uint256
[`seth --to-bytes32`]: #seth---to-bytes32
[`seth --to-address`]: #seth---to-address
[`seth gas-price`]: #seth-gas-price
[`seth abi`]: #seth-abi
[`seth age`]: #seth-age
[`seth balance`]: #seth-balance
[`seth balance`]: #seth-balance
[`seth block`]: #seth-block
[`seth call`]: #seth-call
[`seth calldata`]: #seth-calldata
[`seth chain`]: #seth-chain
[`seth code`]: #seth-code
[`seth estimate`]: #seth-estimate
[`seth events`]: #seth-events
[`seth help`]: #seth-help
[`seth keccak`]: #seth-keccak
[`seth logs`]: #seth-logs
[`seth ls`]: #seth-ls
[`seth ls`]: #seth-ls
[`seth mktx`]: #seth-mktx
[`seth nonce`]: #seth-nonce
[`seth publish`]: #seth-publish
[`seth receipt`]: #seth-receipt
[`seth send`]: #seth-send
[`seth sign`]: #seth-sign
[`seth storage`]: #seth-storage
[`seth tx`]: #seth-tx

[`token`]: https://github.com/dapphub/token

[MakerDAO]: https://makerdao.com
[Dai stablecoin system]: https://makerdao.com
[Dai]: https://makerdao.com
[OasisDEX]: https://oasisdex.com
[`ethsign`]: https://github.com/dapphub/dapptools/blob/master/src/ethsign/README

[Ledger Wallet Linux instructions]: https://ledger.zendesk.com/hc/en-us/articles/115005165269-What-if-Ledger-Wallet-is-not-recognized-on-Linux-
