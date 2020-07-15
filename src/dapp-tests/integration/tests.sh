#!/usr/bin/env bash

set -e

# clean up
trap 'kill -9 $PID_GETH && rm -rf "$TMPDIR"' EXIT

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
  PID_GETH=$!
  # give it a few secs to start up
  sleep 5
  read -r ACC BAL <<< "$(seth ls --keystore "$TMPDIR"/8545/keystore)"
  # The account has maximum balance
  [[ $(seth --to-hex "$BAL") = $(seth --to-int256 -1) ]] || error

  # Deploy a simple contract:
  solc --bin --bin-runtime stateful.sol -o "$TMPDIR"

  A_ADDR=$(seth send --create "$(<"$TMPDIR"/A.bin)" "constructor(uint y)" 1 --from "$ACC" --keystore "$TMPDIR"/8545/keystore --password /dev/null --gas 0xffffffff)

  # Compare deployed code with what solc gives us
  [[ $(seth code "$A_ADDR") = 0x"$(cat "$TMPDIR"/A.bin-runtime)" ]] || error

  # And with what hevm gives us
  EXTRA_CALLDATA=$(seth --to-uint256 1)
  HEVM_RET=$(hevm exec --code "$(<"$TMPDIR"/A.bin)""${EXTRA_CALLDATA/0x/}" --gas 0xffffffff)

  [[ $(seth code "$A_ADDR") = "$HEVM_RET" ]] || error

  TX=$(seth send "$A_ADDR" "off()" --gas 0xffff --password /dev/null --from "$ACC" --keystore "$TMPDIR"/8545/keystore --async)

  # since we have one tx per block, seth run-tx and seth debug are equivalent
  [[ $(seth run-tx "$TX") = 0x ]] || error
}

dapp_testnet

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
