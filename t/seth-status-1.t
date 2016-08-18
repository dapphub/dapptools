#!/usr/bin/env bash
seth --test-stdout 10<<. seth --test-jsonrpc 20<<. seth status
block           1529686/1532348 [9%]
chain           1/Frontier/Homestead
client          Geth/v1.3.5/linux/go1.5.1 PV63
gas-price       20.000000000 Gwei
mining          disabled
network         12 peers
.
[["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
 ["eth_protocolVersion", [], 63],
 ["net_version", [], 1],
 ["eth_gasPrice", [], "0x4a817c800"],
 ["eth_hashrate", [], "0x817c"],
 ["net_peerCount", [], "0xc"],
 ["eth_mining", [], false],
 ["net_listening", [], true],
 ["eth_blockNumber", [], "0x1755d0"],
 ["eth_getBlockByNumber", ["0x0", false],
  { "hash": "0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3" }],
 ["eth_syncing", [], {
   "currentBlock": "0x175756",
   "highestBlock": "0x1761bc",
   "startingBlock": "0x17564d"
 }]]
.
