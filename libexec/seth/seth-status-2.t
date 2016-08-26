#!/usr/bin/env bash
seth --test-output 10<<. seth --test-rpc 20<<. seth status
block	1529296
chain	morden:2/Morden
client	Geth/v1.3.5/linux/go1.5.1
hash	0x18af98dbf2a44c3915e0b7b0e8005fa2eed09b94091bb026483bdbe521e89bc9
mining	33148 hash/s
online	no
price	20.000000000 Gwei/gas
time	2016-08-14T21:34:13+00:00
.
[["eth_getBlockByNumber", ["0x1755d0", false], { "timestamp": "1471210453", "hash": "0x18af98dbf2a44c3915e0b7b0e8005fa2eed09b94091bb026483bdbe521e89bc9" }],
 ["web3_clientVersion", [], "Geth/v1.3.5/linux/go1.5.1"],
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
