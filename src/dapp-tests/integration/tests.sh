#!/usr/bin/env bash

set -ex

# clean up
trap 'killall geth && rm -rf "$TMPDIR"' EXIT
trap "exit 1" SIGINT SIGTERM

error() {
    printf 1>&2 "fail: function '%s' at line %d.\n" "${FUNCNAME[1]}"  "${BASH_LINENO[0]}"
    printf 1>&2 "got: %s" "$output"
    exit 1
}

# tests some of the behaviour of
# `dapp testnet`
# `seth ls`
# `seth send`
# `seth run-tx`
# `hevm exec`
dapp_testnet() {
  TMPDIR=$(mktemp -d)

  dapp testnet --dir "$TMPDIR" &
  # give it a few secs to start up
  sleep 180
  read -r ACC BAL <<< "$(seth ls --keystore "$TMPDIR/8545/keystore")"
  # The account has maximum balance
  [[ $(seth --to-hex "$BAL") = $(seth --to-int256 -1) ]] || error

  # Deploy a simple contract:
  solc --bin --bin-runtime stateful.sol -o "$TMPDIR"

  A_ADDR=$(seth send --create "$(<"$TMPDIR"/A.bin)" "constructor(uint y)" 1 --from "$ACC" --keystore "$TMPDIR"/8545/keystore --password /dev/null --gas 0xffffff)

  # Compare deployed code with what solc gives us
  [[ $(seth code "$A_ADDR") = 0x"$(cat "$TMPDIR"/A.bin-runtime)" ]] || error

  # And with what hevm gives us
  EXTRA_CALLDATA=$(seth --to-uint256 1)
  HEVM_RET=$(hevm exec --code "$(<"$TMPDIR"/A.bin)""${EXTRA_CALLDATA/0x/}" --gas 0xffffff)

  [[ $(seth code "$A_ADDR") = "$HEVM_RET" ]] || error

  TX=$(seth send "$A_ADDR" "off()" --gas 0xffff --password /dev/null --from "$ACC" --keystore "$TMPDIR"/8545/keystore --async)

  # since we have one tx per block, seth run-tx and seth debug are equivalent
  [[ $(seth run-tx "$TX") = 0x ]] || error

  # dynamic fee transaction (EIP-1559)
  seth send "$A_ADDR" "on()" --gas 0xffff --password /dev/null --from "$ACC" --keystore "$TMPDIR"/8545/keystore --prio-fee 2gwei --gas-price 10gwei

  B_ADDR=$(seth send --create 0x647175696e6550383480393834f3 --gas 0xffff --password /dev/null --from "$ACC" --keystore "$TMPDIR"/8545/keystore --prio-fee 2gwei --gas-price 10gwei)

  [[ $(seth code "$B_ADDR") = 0x647175696e6550383480393834f3 ]] || error

  # clean up
  killall geth
}

dapp_testnet

# checks that seth send works with both checksummed and unchecksummed addresses
seth_send_address_formats() {
  TMPDIR=$(mktemp -d)

  dapp testnet --dir "$TMPDIR" &
  # give it a few secs to start up
  sleep 180
  read -r ACC BAL <<< "$(seth ls --keystore "$TMPDIR/8545/keystore")"

  lower=$(echo "$ACC" | tr '[:upper:]' '[:lower:]')
  export ETH_GAS=0xffff

  zero=0x0000000000000000000000000000000000000000

  # with checksummed
  tx=$(seth send "$zero" --from "$ACC" --password /dev/null --value "$(seth --to-wei 1 ether)" --keystore "$TMPDIR"/8545/keystore --async)
  [[ $(seth tx "$tx" from) = "$lower" ]]

  # without checksum
  tx=$(seth send "$zero" --from "$lower" --password /dev/null --value "$(seth --to-wei 1 ether)" --keystore "$TMPDIR"/8545/keystore --async)
  [[ $(seth tx "$tx" from) = "$lower" ]]

  # try again with eth_rpc_accounts
  export ETH_RPC_ACCOUNTS=true

  # with checksummed
  tx=$(seth send "$zero" --from "$ACC" --password /dev/null --value "$(seth --to-wei 1 ether)" --keystore "$TMPDIR"/8545/keystore --async)
  [[ $(seth tx "$tx" from) = "$lower" ]]

  # without checksum
  tx=$(seth send "$zero" --from "$lower" --password /dev/null --value "$(seth --to-wei 1 ether)" --keystore "$TMPDIR"/8545/keystore --async)
  [[ $(seth tx "$tx" from) = "$lower" ]]

  # clean up
  killall geth
}

seth_send_address_formats

test_hevm_symbolic() {
    solc --bin-runtime -o . --overwrite factor.sol
    # should find counterexample
    hevm symbolic --code "$(<A.bin-runtime)" --sig "factor(uint x, uint y)" --smttimeout 40000 --solver cvc4 && error || echo "hevm success: found counterexample"
    rm -rf A.bin-runtime
    hevm symbolic --code "$(<dstoken.bin-runtime)" --sig "transferFrom(address, address, uint)" --get-models

    solc --bin-runtime -o . --overwrite token.sol
    # This one explores all paths (cvc4 is better at this)
    hevm symbolic --code "$(<Token.bin-runtime)" --solver cvc4
    rm -rf Token.bin-runtime

    # The contracts A and B should be equivalent:
    solc --bin-runtime -o . --overwrite AB.sol
    hevm equivalence --code-a "$(<A.bin-runtime)" --code-b "$(<B.bin-runtime)" --solver cvc4
    rm -rf A.bin-runtime B.bin-runtime
}

test_hevm_symbolic

test_custom_solc_json() {
    TMPDIR=$(mktemp -d)

    # copy source file
    mkdir -p "$TMPDIR/src"
    cp factor.sol "$TMPDIR/src"

    # init dapp project
    cd "$TMPDIR"
    export GIT_CONFIG_NOSYSTEM=1
    export GIT_AUTHOR_NAME=dapp
    export GIT_AUTHOR_EMAIL=dapp@hub.lol
    export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
    export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
    dapp init

    # init custom json
    dapp mk-standard-json > config.json

    # build with custom json
    DAPP_STANDARD_JSON="config.json" dapp build
}

test_custom_solc_json

# SETH CALLDATA TESTS
test_calldata_1() {
    local output
    output=$(seth --to-uint256 1 )
    [[ $output = "0x0000000000000000000000000000000000000000000000000000000000000001" ]] || error
}
test_calldata_1

test_calldata_2() {
    local output
    output=$(seth calldata 'bar(bool)' false)

    [[ $output = "0x6fae94120000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_2

test_calldata_3() {
    local output
    output=$(seth calldata 'f(bytes[])' '[0x01, 0x01]')

    [[ $output = "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_3

test_calldata_4() {
    local output
    output=$(seth calldata 'f(uint a)' '1')

    [[ $output = "0xb3de648b0000000000000000000000000000000000000000000000000000000000000001" ]] || error
}
test_calldata_4

test_calldata_5() {
    local output
    output=$(seth calldata 'f(uint a)' '0x01')

    [[ $output = "0xb3de648b0000000000000000000000000000000000000000000000000000000000000001" ]] || error
}
test_calldata_5

test_calldata_6() {
    local output
    output=$(seth calldata 'f(bool[], uint)' '[false, true]' 1)

    [[ $output = "0x7abab09100000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001" ]] || error
}
test_calldata_6

test_calldata_7() {
    local output
    output=$(seth calldata 'f(bytes)' 0x01)

    [[ $output = "0xd45754f8000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_7

test_calldata_8() {
    local output
    output=$(seth calldata 'f(bytes[])' '[0x01]')

    [[ $output = "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_8

test_calldata_9() {
    local output
    output=$(seth calldata 'f(bytes[])' '[]')

    [[ $output = "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_9

test_calldata_10() {
    local output
    output=$(seth calldata 'foo(bytes)' '0x')
    [[ $output = "0x30c8d1da00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_10

test_calldata_11() {
    local output
    output=$(seth calldata 'foo(bytes[])' '[0x,0x]')

    [[ $output = "0x36fe9f8d000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_11

test_calldata_12() {
    local output
    output=$(seth calldata 'foo(bytes[])' '[0x12, 0x]')

    [[ $output = "0x36fe9f8d0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_12

test_calldata_13() {
    local output
    output=$(seth calldata 'f(uint a)' "$(seth --to-int256 -1)")

    [[ $output = "0xb3de648bffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" ]] || error
}
test_calldata_13


test_calldata_14() {
    local output
    output=$(seth calldata 'f(uint[][])' '[[1],[2,3]]')

    [[ $output = "0xc26b6b9a000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003" ]] || error
}
test_calldata_14

test_calldata_15() {
    local output
    output=$(seth calldata 'f(bool[][] yolo)' '[[false, true], [false]]')

    [[ $output = "0x9775f34d00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_15


test_calldata_16() {
    local output
    output=$(seth calldata 'foo(string token)' '"hey"')

    [[ $output = "0xf31a6969000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000036865790000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_16

test_calldata_17() {
    local output
    output=$(seth calldata 'foo(string token)' '"  hey"')

    [[ $output = "0xf31a6969000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000052020686579000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_17


test_calldata_18() {
    local output
    output=$(seth calldata 'foo(string[])' '["  hey","sad",""]')

    [[ $output = "0x223f0b6000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000052020686579000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000373616400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_calldata_18

test_calldata_19() {
    local output
    output=$(seth calldata 'foo()')

    [[ $output = "0xc2985578" ]] || error
}
test_calldata_19

test_keccak_1() {
  local output
  output=$(seth keccak 0xcafe)

  [[ $output = "0x72318c618151a897569554720f8f1717a3da723042fb73893c064da11b308ae9" ]] || error
}
test_keccak_1

test_keccak_2() {
  local output
  output=$(seth keccak 0xca:0xfe)

  [[ $output = "0x72318c618151a897569554720f8f1717a3da723042fb73893c064da11b308ae9" ]] || error
}
test_keccak_2

test_keccak_3() {
  local output
  output=$(seth keccak cafe)

  [[ $output = "0x4c84268b4bd90011342a28648371055c58267a2a8e93a7b0bc61fe93bf186974" ]] || error
}
test_keccak_3

test_hexdata_0() {
  [[ $(seth --to-hexdata cafe) = "0xcafe" ]] || error
}
test_hexdata_0

test_hexdata_1() {
  [[ $(seth --to-hexdata 0xcafe) = "0xcafe" ]] || error
}
test_hexdata_1

test_hexdata_2() {
  [[ $(seth --to-hexdata 0xCA:0xfe) = "0xcafe" ]] || error
}
test_hexdata_2

test_hexdata_3() {
  [[ $(seth --to-hexdata 0xCA:0xfe:0x) = "0xcafe" ]] || error
}
test_hexdata_3

# SETH ENS TESTS
# Tests for resolve-name and lookup-address use a Rinkeby name that's been registered for 100 years and will not be changed
# Infura ID source: https://github.com/ethers-io/ethers.js/blob/0d40156fcba5be155aa5def71bcdb95b9c11d889/packages/providers/src.ts/infura-provider.ts#L17
ETH_RPC_URL=https://rinkeby.infura.io/v3/84842078b09946638c03157f83405213

test_namehash_1() {
    local output
    output=$(seth namehash)
    [[ $output = "0x0000000000000000000000000000000000000000000000000000000000000000" ]] || error
}
test_namehash_1

test_namehash_2() {
    local output
    output=$(seth namehash eth)
    [[ $output = "0x93cdeb708b7545dc668eb9280176169d1c33cfd8ed6f04690a0bcc88a93fc4ae" ]] || error
}
test_namehash_2

test_namehash_3() {
    local output
    output=$(seth namehash ricmoo.firefly.eth)
    [[ $output = "0x0bcad17ecf260d6506c6b97768bdc2acfb6694445d27ffd3f9c1cfbee4a9bd6d" ]] || error
}
test_namehash_3

test_namehash_4() {
    local output
    output=$(seth namehash seth-test.eth)
    [[ $output = "0xc639cb4715c456d2cc8523ee5568222dbae3551e2a42c61a7da4db3ec28ab9e9" ]] || error
}
test_namehash_4

test_namehash_5() {
    [[ $(seth namehash eth) = $(seth namehash ETH) ]] || error
}
test_namehash_5

test_namehash_6() {
    [[ $(seth namehash ricmoo.firefly.eth) = $(seth namehash RicMOO.FireFly.eTH) ]] || error
}
test_namehash_6

test_namehash_7() {
    [[ $(seth namehash seth-test.eth) = $(seth namehash sEtH-tESt.etH) ]] || error
}
test_namehash_7

test-resolve-name1() {
    # using example from ethers docs: https://docs.ethers.io/v5/single-page/#/v5/api/providers/provider/-%23-Provider-ResolveName
    local output
    output=$(seth resolve-name seth-test.eth --rpc-url=$ETH_RPC_URL)
    [[ $output = "0x49c92F2cE8F876b070b114a6B2F8A60b83c281Ad" ]] || error
}
test-resolve-name1

test-resolve-name2() {
    [[ $(seth resolve-name seth-test.eth --rpc-url=$ETH_RPC_URL) = $(seth resolve-name sEtH-tESt.etH --rpc-url=$ETH_RPC_URL) ]] || error
}
test-resolve-name2

test-lookup-address1() {
    # using example from ethers docs: https://docs.ethers.io/v5/single-page/#/v5/api/providers/provider/-%23-Provider-lookupAddress
    local output
    output=$(seth lookup-address 0x49c92F2cE8F876b070b114a6B2F8A60b83c281Ad --rpc-url=$ETH_RPC_URL)
    [[ $output = "seth-test.eth" ]] || error
}
test-lookup-address1

test-lookup-address2() {
    [[ $(seth lookup-address 0x49c92F2cE8F876b070b114a6B2F8A60b83c281Ad --rpc-url=$ETH_RPC_URL) \
     = $(seth lookup-address 0x49c92f2ce8f876b070b114a6b2f8a60b83c281ad --rpc-url=$ETH_RPC_URL) ]] || error
}
test-lookup-address2

# SETH FIXED POINT TESTS
# seth --from-fix
test-from-fix1() {
    [[ $(seth --from-fix 6 1) = 1000000 ]] || error
}
test-from-fix1

test-from-fix2() {
    [[ $(seth --from-fix 18 1) = 1000000000000000000 ]] || error
}
test-from-fix2

test-from-fix3() {
    [[ $(seth --from-fix 6 1.2345) = 1234500 ]] || error
}
test-from-fix3

test-from-fix4() {
    [[ $(seth --from-fix 18 1.23456789) = 1234567890000000000 ]] || error
}
test-from-fix4

# seth --to-fix
test-to-fix1() {
    [[ $(seth --to-fix 6 1000000) = 1.000000 ]] || error
}
test-to-fix1

test-to-fix2() {
    [[ $(seth --to-fix 18 1000000000000000000) = 1.000000000000000000 ]] || error
}
test-to-fix2

test-to-fix3() {
    [[ $(seth --to-fix 6 1234500) = 1.234500 ]] || error
}
test-to-fix3

test-to-fix4() {
    [[ $(seth --to-fix 18 1234567890000000000) = 1.234567890000000000 ]] || error
}
test-to-fix4