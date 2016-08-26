#!/usr/bin/env bash
seth --test-output 10<<. seth --test-rpc 20<<. seth account
0x841b4d218f211f4e21e856e89d42521b2211201d	0.000000000000000000 ETH
.
[["eth_accounts", [], ["0x841b4d218f211f4e21e856e89d42521b2211201d"]],
 ["eth_getBalance", ["0x841b4d218f211f4e21e856e89d42521b2211201d", "latest"], "0x0"]]
.
