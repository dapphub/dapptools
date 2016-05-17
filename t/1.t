#!/bin/sh -e
alias ok=seth-test-ok

ok 3<<: 4<<. seth accounts
[["eth_accounts", [], ["0x841b4d218f211f4e21e856e89d42521b2211201d"]],
 ["eth_getBalance", ["0x841b4d218f211f4e21e856e89d42521b2211201d"], "0x0"]]
:
0x841b4d218f211f4e21e856e89d42521b2211201d      0
.

ok 3<<: 4<<. seth status
[["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
 ["eth_protocolVersion", [], 63],
 ["net_version", [], 1],
 ["eth_gasPrice", [], "0x4a817c800"],
 ["eth_hashrate", [], "0x817c"],
 ["net_peerCount", [], "0xc"],
 ["eth_mining", [], false],
 ["net_listening", [], true],
 ["eth_blockNumber", [], "0x1755d0"],
 ["eth_syncing", [], {
   "currentBlock": "0x175756",
   "highestBlock": "0x1761bc",
   "startingBlock": "0x17564d"
 }]]
:
block           1529686 (265/2927 of 1532348 [9%])
gas-price       20000000000
mining          disabled
network         mainnet (12 peers, listening)
version         Geth/v1.3.5/linux/go1.5.1 (PV63)
.

ok 3<<: 4<<. seth status
[["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
 ["eth_protocolVersion", [], 62],
 ["net_version", [], 0],
 ["eth_gasPrice", [], "0x4a817c800"],
 ["eth_hashrate", [], "0x817c"],
 ["net_peerCount", [], "0x5"],
 ["eth_mining", [], true],
 ["net_listening", [], false],
 ["eth_blockNumber", [], "0x1755d0"],
 ["eth_syncing", [], false]]
:
block           1529296
gas-price       20000000000
mining          33148 hash/s
network         testnet (5 peers)
version         Geth/v1.3.5/linux/go1.5.1 (PV62)
.
