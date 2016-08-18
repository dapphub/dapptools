#!/usr/bin/env bash
seth --test-stdout 10<<. seth --test-jsonrpc 20<<. seth status
block           1529296
chain           2/Morden
client          Geth/v1.3.5/linux/go1.5.1 PV62
gas-price       20.000000000 Gwei
mining          33148 hash/s
network         5 peers
.
[["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
 ["eth_protocolVersion", [], 62],
 ["net_version", [], 2],
 ["eth_gasPrice", [], "0x4a817c800"],
 ["eth_hashrate", [], "0x817c"],
 ["net_peerCount", [], "0x5"],
 ["eth_mining", [], true],
 ["net_listening", [], false],
 ["eth_blockNumber", [], "0x1755d0"],
 ["eth_getBlockByNumber", ["0x0", false],
  { "hash": "0x0cd786a2425d16f152c658316c423e6ce1181e15c3295826d7c9904cba9ce303" }],
 ["eth_syncing", [], false]]
.
