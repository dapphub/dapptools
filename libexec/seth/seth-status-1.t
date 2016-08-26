#!/usr/bin/env bash
seth --test-output 10<<. seth --test-rpc 20<<. seth status
block	1529686/1532348 [9%]
chain	unknown:1/Frontier/Homestead/Unforked (ETH)
client	Geth/v1.3.5/linux/go1.5.1
hash	0x18af98dbf2a44c3915e0b7b0e8005fa2eed09b94091bb026483bdbe521e89bc9
mining	no
online	yes [12 peers]
price	20.000000000 Gwei/gas
time	2016-08-14T21:34:13+00:00
.
[["eth_getBlockByNumber", ["0x1755d0", false], { "timestamp": "1471210453", "hash": "0x18af98dbf2a44c3915e0b7b0e8005fa2eed09b94091bb026483bdbe521e89bc9" }],
 ["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
 ["eth_protocolVersion", [], 63],
 ["net_version", [], 1],
 ["eth_gasPrice", [], "0x4a817c800"],
 ["eth_hashrate", [], "0x817c"],
 ["net_peerCount", [], "0xc"],
 ["eth_mining", [], false],
 ["net_listening", [], true],
 ["eth_blockNumber", [], "0x1755d0"],
 ["eth_getBlockByNumber", ["0x0", false], { "hash": "0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3" }],
 ["eth_syncing", [], { "currentBlock": "0x175756", "highestBlock": "0x1761bc", "startingBlock": "0x17564d" }]]
.
