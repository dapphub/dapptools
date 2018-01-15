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
hardware wallets.

> "One indicator I look for in a healthy open source project is how
  many useful tools come out of its team as a side effect of their
  efforts."
  —[`@danfinlay`](https://twitter.com/danfinlay/status/942909341044162560])
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
      * [Your address](#your-address)
  * [Basic usage: a tutorial](#basic-usage-a-tutorial)
      * [Ether transactions](#ether-transactions)
      * [Helper commands](#helper-commands)
      * [Checking ether balances](#checking-ether-balances)
      * [Reading from contracts](#reading-from-smart-contracts)
      * [Transacting with contracts](#transacting-with-contracts)

<br />

Installing
------------------------------------------------------------------------

Seth is distributed via [the Nix package manager], enabling
cryptographically precise dependency tracking.  First, install Nix
itself:

    $ curl https://nixos.org/nix/install | sh

Then add DappHub's distribution channel and install `seth`:

    $ nix-channel --add https://nix.dapphub.com/pkgs/dapphub
    $ nix-channel --update
    $ nix-env -iA dapphub.seth

See [dapp.tools](https://dapp.tools) for more software available
through our channel.

### Upgrading

To upgrade Seth to the latest release, update the channel and then
reinstall:

    $ nix-channel --update
    $ nix-env -iA dapphub.seth

<br />

Configuration
------------------------------------------------------------------------

Seth has options that can be specified via command-line flags
or environment variables.

For convenience, Seth looks for `~/.sethrc` and loads it as a shell
script (Bash 4 syntax).  This is a convenient place to set default
options by exporting environment variables.

### Example `.sethrc` file

The `~/.sethrc` file is just a regular Bash script that is
automatically loaded by Seth.  Here is an example:

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
[Infura]) using the variable `SETH_CHAIN` or the flag `--chain`.
Allowed values: `ethlive` (aka `mainnet`), `ropsten`, `kovan`, and
`rinkeby`.

### Key management and signing

By default, Seth does not use the RPC node for key management or
signing transactions.  Instead, it uses keys stored on your machine,
as well as your Ledger Nano S hardware wallet (if present).  **Thus,
you do not need to "unlock" your account in Geth or Parity.**

Seth looks for keys in the standard directories of Geth and Parity.
To configure a custom location for your key files, use the
`ETH_KEYSTORE` variable or the `--keystore` flag.

If your key is protected with a password, Seth will prompt you each
time you make a transaction.  If you are confident in your computer
security, and you want to (say) run a bot script, you can set the
`ETH_PASSWORD` variable (flag: `--password`) to point to a file
containing your password.

If you do want to use the RPC node for key management and signing, set
the `ETH_RPC_ACCOUNTS` variable or use the `--rpc-accounts` flag.
This probably means you need to use Geth's or Parity's account
management tools to "unlock" your account.

### Your address

When making transactions or doing read-only calls, Seth takes the
sending address from the `ETH_FROM` variable or the `--from` flag.

Note for Ledger Nano S hardware wallet users: Seth currently only
looks for the first four addresses derived from your seed phrase.
If the sending address is not one of those, Seth will not be able
to sign transactions.

<br />

Basic usage: a tutorial
------------------------------------------------------------------------

This section assumes that you have something like the example
`.sethrc` file specifying how to connect to the blockchain
and a default sender address.

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
wei amount using [`seth --to-wei`](#seth-to-wei).  Here, we send 1.5
ETH:

    $ fund=0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    $ seth send --value $(seth --to-wei 1.5 eth) $fund

There is also [`seth --from-wei`] for converting wei amounts into a
more readable notation.

For more advanced blockchain interactions, the helpers [`seth
--abi-decode`], [`seth --from-ascii`], and [`seth --from-bin`] are
also useful.

### Checking ether balances

Now you can use [`seth balance`](#seth-balance) to see how much is in
the donation fund:

    $ seth balance 0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    2963.72865500027557173E+18

You can use [`seth ls`](#seth-ls) to check the ether balances of your
own accounts:

    $ seth ls
    0xCC41D9831E4857B4F16914A356306fBeA734183A    0.24E+18
    0xD9ceccea2BEE9a367d78658aBbB2Fe979b3877Ef    0.03409E+18

### Reading from contracts

The basic tool to read information from a contract is [seth call],
which performs a call without publishing a transaction.

For example, you can read the total supply parameter the [MakerDAO]
fund using the ERC20 ABI:

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
    $ amount=$(seth --from-wei 0.5 ether)
    $ seth send $DAI "approve(address,uint256)" $OASIS $amount

(Again, see [`token`] for a more convenient way to interact with
ERC20 tokens.)

See [`seth send`] for details on passing arguments, doing asynchronous
transactions, and so on.


[the DappHub collective]: https://dapphub.com

[Ledger Nano S]: https://www.ledgerwallet.com/products/ledger-nano-s

[Infura]: https://infura.io

["command substitution"]: https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html

[Ethereum Foundation's donation address]: https://www.ethereum.org/donate

[the Nix package manager]: https://nixos.org/nix

[MakerDAO CDP utility]: https://github.com/makerdao/dai-cli

[`seth --abi-decode`]: #seth-abi-decode
[`seth --from-ascii`]: #seth-from-ascii
[`seth --from-bin`]: #seth-from-bin
[`seth --from-wei`]: #seth-from-wei
[`seth --to-wei`]: #seth-to-wei
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
[`seth send`]: #seth-send
[`seth sign`]: #seth-sign
[`seth storage`]: #seth-storage
[`seth tx`]: #seth-tx

[`token`]: https://github.com/dapphub/token

[MakerDAO]: https://makerdao.com
[Dai stablecoin system]: https://makerdao.com
[Dai]: https://makerdao.com
[OasisDEX]: https://oasisdex.com
