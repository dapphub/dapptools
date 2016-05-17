#!/bin/sh -e
scenario=syncing \
seth test 10<<. seth mock-rpc 20<<. seth status
block           1529686/1532348 [99%]
chain           1/Frontier/Homestead
client          Geth/v1.3.5/linux/go1.5.1 PV63
gas-price       20000000000
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
 ["eth_syncing", [], {
   "currentBlock": "0x175756",
   "highestBlock": "0x1761bc",
   "startingBlock": "0x17564d"
 }]]
.
scenario=mining \
seth test 10<<. seth mock-rpc 20<<. seth status
block           1529296
chain           2/Morden
client          Geth/v1.3.5/linux/go1.5.1 PV62
gas-price       20000000000
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
 ["eth_syncing", [], false]]
.
