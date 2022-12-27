#! /usr/bin/env bash

set -eo pipefail

# ------------------------------------------------
#                CONFIGURATION
# ------------------------------------------------

export SKIP_SETUP=${SKIP_SETUP:-0}
export FUZZ_RUNS=${FUZZ_RUNS:-100}
export TESTNET_SLEEP=${TESTNET_SLEEP:-5}
export ARCHIVE_NODE_URL=${ARCHIVE_NODE_URL:-https://eth-mainnet.alchemyapi.io/v2/vpeKFsEF6PHifHzdtcwXSDbhV3ym5Ro4}
export ETHERSCAN_API_KEY=${ETHERSCAN_API_KEY:-15IS6MMRAYB19NZN9VHH6H6P57892Z664M}

# ------------------------------------------------
#                SHARED SETUP
# ------------------------------------------------

# we spin up a new testnet instance and share it between all tests
setup_suite() {
    if [[ "$SKIP_SETUP" != 1 ]]; then
        export GETHDIR
        GETHDIR=$(mktemp -d)

        dapp testnet --dir "$GETHDIR" &
        # give it a few secs to start up
        sleep "$TESTNET_SLEEP"

        export ETH_RPC_URL="http://127.0.0.1:8545"
        export ETH_KEYSTORE="$GETHDIR/8545/keystore"
        export ETH_PASSWORD=/dev/null
        read -r ROOT _ <<< "$(seth ls --keystore "$GETHDIR/8545/keystore")"
    fi
}

# cleanup the testnet
teardown_suite() {
    if [[ "$SKIP_SETUP" != 1 ]]; then
        killall geth
        rm -rf "$GETHDIR"
    fi
}

# ------------------------------------------------
#                TEST HELPERS
# ------------------------------------------------

# generates a new account and gives it some eth, returns the address
fresh_account() {
    wei_amount=${1:-$(seth --to-wei 42069 ether)}
    output=$(geth account new --password /dev/null --keystore "$ETH_KEYSTORE")
    account=$(echo "$output" | grep "Public address of the key" | awk 'NF>1{print $NF}')
    seth send -F "$ROOT" -V "$wei_amount" "$account" 1>&2
    echo "$account"
}

# ensure that fresh_account does what it should
test_funding() {
    # shellcheck disable=SC2119
    acc=$(fresh_account)
    assert_equals "$(seth --to-wei 42069 ether)" "$(seth balance "$acc")"

    acc=$(fresh_account "$(seth --to-wei 100 ether)")
    assert_equals "$(seth --to-wei 100 ether)" "$(seth balance "$acc")"
}

# a few useful addresses
VITALIK=0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045
ZERO=0x0000000000000000000000000000000000000000

# location of the test contracts
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
CONTRACTS="$SCRIPT_DIR/contracts"

# ------------------------------------------------
#                 GENERATORS
# ------------------------------------------------

mod() {
    bc <<< "$1%$2"
}

hex2dec() {
    echo "ibase=16;${1^^}" | bc | tr -d '\\\n'
}

byte() {
    hexdump -n 1 -e '1/1 "%08X" 1 "\n"' /dev/urandom
}

bytes32() {
    hexdump -n 32 -e '4/8 "%08X" 1 "\n"' /dev/urandom
}

uint8() {
    hex2dec "$(byte)"
}

uint256() {
    hex2dec "$(bytes32)"
}

alpha() {
    local size
    size=$1
    tr -dc '[:alpha:]' < /dev/urandom | fold -w "${1:-$size}" | head -n 1
}

# ------------------------------------------------
#                   TESTS
# ------------------------------------------------

# tests some of the behaviour of
# `seth send`
# `seth run-tx`
# `hevm exec`
test_smoke() {
    local account bytecode
    account=$(fresh_account)
    bytecode=$(mktemp -d)

    # Deploy a simple contract:
    solc --bin --bin-runtime "$CONTRACTS/stateful.sol" -o "$bytecode"

    A_ADDR=$(seth send --create "$(<"$bytecode"/A.bin)" "constructor(uint y)" 1 --from "$account" --gas 0xffffff)

    # Compare deployed code with what solc gives us
    assert_equals 0x"$(cat "$bytecode"/A.bin-runtime)" "$(seth code "$A_ADDR")"

    # And with what hevm gives us
    EXTRA_CALLDATA=$(seth --to-uint256 1)
    HEVM_RET=$(hevm exec --code "$(<"$bytecode"/A.bin)""${EXTRA_CALLDATA/0x/}" --gas 0xffffff)

    assert_equals "$HEVM_RET" "$(seth code "$A_ADDR")"

    TX=$(seth send "$A_ADDR" "off()" --gas 0xffff --password /dev/null --from "$account" --async)

    # since we have one tx per block, seth run-tx and seth debug are equivalent
    assert_equals 0x "$(seth run-tx "$TX" --no-src)"

    # dynamic fee transactions (EIP-1559)
    seth send "$A_ADDR" "on()" \
        --gas 0xffff \
        --password /dev/null \
        --from "$account" \
        --prio-fee 2gwei \
        --gas-price 10gwei

    B_ADDR=$(seth send --create 0x647175696e6550383480393834f3 \
        --gas 0xffff \
        --password /dev/null \
        --from "$account" \
        --prio-fee 2gwei \
        --gas-price 10gwei)

    assert_equals 0x647175696e6550383480393834f3 "$(seth code "$B_ADDR")"
}

# checks that seth send works with both checksummed and unchecksummed addresses
test_seth_send_address_formats() {
    local account
    acc=$(fresh_account)

    lower=$(echo "$acc" | tr '[:upper:]' '[:lower:]')
    export ETH_GAS=0xffff

    # with checksummed
    tx=$(seth send "$ZERO" --from "$acc" --password /dev/null --value "$(seth --to-wei 1 ether)" --async)
    assert_equals "$lower" "$(seth tx "$tx" from)"

    # without checksum
    tx=$(seth send "$ZERO" --from "$lower" --password /dev/null --value "$(seth --to-wei 1 ether)" --async)
    assert_equals "$lower" "$(seth tx "$tx" from)"

    # try again with eth_rpc_accounts
    export ETH_RPC_ACCOUNTS=true

    # with checksummed
    tx=$(seth send "$ZERO" --from "$acc" --password /dev/null --value "$(seth --to-wei 1 ether)" --async)
    assert_equals "$lower" "$(seth tx "$tx" from)"

    # without checksum
    tx=$(seth send "$ZERO" --from "$lower" --password /dev/null --value "$(seth --to-wei 1 ether)" --async)
    assert_equals "$lower" "$(seth tx "$tx" from)"
}


test_hevm_symbolic() {
    cd "$(mktemp -d)" || exit

    solc --bin-runtime -o . --overwrite "$CONTRACTS/factor.sol"
    # should find counterexample
    hevm symbolic --code "$(<A.bin-runtime)" --sig "factor(uint x, uint y)" --smttimeout 40000 --solver cvc4 && fail || echo "hevm success: found counterexample"
    hevm symbolic --code "$(<"$CONTRACTS/dstoken.bin-runtime")" --sig "transferFrom(address, address, uint)" --get-models &> /dev/null || fail

    solc --bin-runtime -o . --overwrite "$CONTRACTS/token.sol"
    # This one explores all paths (cvc4 is better at this)
    hevm symbolic --code "$(<Token.bin-runtime)" --solver cvc4 || fail

    # The contracts A and B should be equivalent:
    solc --bin-runtime -o . --overwrite "$CONTRACTS/AB.sol"
    hevm equivalence --code-a "$(<A.bin-runtime)" --code-b "$(<B.bin-runtime)" --solver cvc4 || fail
}

test_custom_solc_json() {
    tmp=$(mktemp -d)

    # copy source file
    mkdir -p "$tmp/src"
    cp "$CONTRACTS/factor.sol" "$tmp/src"

    # init dapp project
    cd "$tmp" || exit
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
    assert "[[ -f out/dapp.sol.json ]]"
}

test_gas_snapshots() {
    tmp=$(mktemp -d)

    # copy source file
    mkdir -p "$tmp/src"
    cp "$CONTRACTS/factor.sol" "$tmp/src"

    # init dapp project
    cd "$tmp" || exit
    export GIT_CONFIG_NOSYSTEM=1
    export GIT_AUTHOR_NAME=dapp
    export GIT_AUTHOR_EMAIL=dapp@hub.lol
    export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
    export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
    dapp init

    # test with snapshots
    dapp snapshot
    assert "[[ -f .gas-snapshot ]]"

    # test check snapshots
    dapp check-snapshot || fail

    # check that check snapshots fails if we change the snapshot
    echo this_will_change_the_snapshot > .gas-snapshot
    dapp check-snapshot && fail || echo "dapp success: snapshot diff detected"
}

test_nonce_1() {
    local account
    account=$(fresh_account)
    assert_equals 0 "$(seth nonce "$account")"

    seth send -F "$account" "$VITALIK"
    assert_equals 1 "$(seth nonce "$account")"
}

test_block_1() {
    local account
    account=$(fresh_account)

    # block number should increase when we send a tx
    assert_equals 1 "$(seth block-number)"
    tx=$(seth send -F "$account" -V "$(seth --to-wei 1 ether)" "$VITALIK" --async)
    assert_equals 2 "$(seth block-number)"

    # block should contain one tx that just sent eth
    assert_equals 21000 "$(seth block 2 gasUsed)"

    # block should contain the tx that we sent before
    txs=$(seth block 2 transactions)
    assert_equals 1 "$(echo "$txs" | jq length)"
    assert_equals "$tx" "$(echo "$txs" | jq -r '.[0]')"
}

test_decimal_roundtrip() {
    for _ in $(seq "$FUZZ_RUNS"); do
      local input
      input=$(uint256)
      assert_equals "$input" "$(seth --to-dec "$(seth --to-hex "$input")")"
    done
}

test_hex_roundtrip() {
    for _ in $(seq "$FUZZ_RUNS"); do
      local input
      input="0x$(bytes32)"
      lower=$(echo "$input" | tr '[:upper:]' '[:lower:]')
      assert_equals "$lower" "$(seth --to-hex "$(seth --to-dec "$input")")"
    done
}

test_to_fix_roundtrip() {
    for _ in $(seq "$FUZZ_RUNS"); do
      local input digits
      input="$(uint256)"

      length="${#input}"
      digits="$(mod "$(uint8)" "$length")"

      assert_equals "$input" "$(seth --from-fix "$digits" "$(seth --to-fix "$digits" "$input")")"
    done
}

test_from_fix_roundtrip() {
    for _ in $(seq "$FUZZ_RUNS"); do
      local input digits
      input="$(uint256)"

      length="${#input}"
      digits="$(mod "$(uint8)" "$length")"

      whole_digits=$(bc <<< "$length - $digits" | tr -d '\\\n')
      input="${input:0:whole_digits}.${input:$whole_digits:$length}"

      assert_equals "$input" "$(seth --to-fix "$digits" "$(seth --from-fix "$digits" "$input")")"
    done
}

test_calldata_1() {
    local output
    output=$(seth --to-uint256 1 )

    assert_equals "0x0000000000000000000000000000000000000000000000000000000000000001" "$output"
}

test_calldata_2() {
    local output
    output=$(seth calldata 'bar(bool)' false)

    assert_equals "0x6fae94120000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_3() {
    local output
    output=$(seth calldata 'f(bytes[])' '[0x01, 0x01]')

    assert_equals "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_4() {
    local output
    output=$(seth calldata 'f(uint a)' '1')

    assert_equals "0xb3de648b0000000000000000000000000000000000000000000000000000000000000001" "$output"
}

test_calldata_5() {
    local output
    output=$(seth calldata 'f(uint a)' '0x01')

    assert_equals "0xb3de648b0000000000000000000000000000000000000000000000000000000000000001" "$output"
}

test_calldata_6() {
    local output
    output=$(seth calldata 'f(bool[], uint)' '[false, true]' 1)

    assert_equals "0x7abab09100000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001" "$output"
}

test_calldata_7() {
    local output
    output=$(seth calldata 'f(bytes)' 0x01)

    assert_equals "0xd45754f8000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_8() {
    local output
    output=$(seth calldata 'f(bytes[])' '[0x01]')

    assert_equals "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_9() {
    local output
    output=$(seth calldata 'f(bytes[])' '[]')

    assert_equals "0xd0b47c0400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_10() {
    local output
    output=$(seth calldata 'foo(bytes)' '0x')

    assert_equals "0x30c8d1da00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_11() {
    local output
    output=$(seth calldata 'foo(bytes[])' '[0x,0x]')

    assert_equals "0x36fe9f8d000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_12() {
    local output
    output=$(seth calldata 'foo(bytes[])' '[0x12, 0x]')

    assert_equals "0x36fe9f8d0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_13() {
    local output
    output=$(seth calldata 'f(uint a)' "$(seth --to-int256 -1)")

    assert_equals "0xb3de648bffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" "$output"
}


test_calldata_14() {
    local output
    output=$(seth calldata 'f(uint[][])' '[[1],[2,3]]')

    assert_equals "0xc26b6b9a000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003" "$output"
}

test_calldata_15() {
    local output
    output=$(seth calldata 'f(bool[][] yolo)' '[[false, true], [false]]')

    assert_equals "0x9775f34d00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000" "$output"
}


test_calldata_16() {
    local output
    output=$(seth calldata 'foo(string token)' '"hey"')

    assert_equals "0xf31a6969000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000036865790000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_17() {
    local output
    output=$(seth calldata 'foo(string token)' '"  hey"')

    assert_equals "0xf31a6969000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000052020686579000000000000000000000000000000000000000000000000000000" "$output"
}


test_calldata_18() {
    local output
    output=$(seth calldata 'foo(string[])' '["  hey","sad",""]')

    assert_equals "0x223f0b6000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000052020686579000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000373616400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_calldata_19() {
    local output
    output=$(seth calldata 'foo()')

    assert_equals "0xc2985578" "$output"
}

test_calldata_20() {
    local output
    output=$(seth calldata 'foo(bytes32, bytes4, bytes16)' '0x' '0x' '0x')

    assert_equals "0x1765a16e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_sig_1() {
    assert_equals "0xd2ce7d65" "$(seth sig 'outboundTransfer(address,address,uint256,uint256,uint256,bytes)')"
}

test_sig_2() {
    assert_equals "0x6a4f4b05" "$(seth sig 'setKeepers(address[],bool[])')"
}

test_sig_3() {
    assert_equals "0xf837dc72" "$(seth sig 'payAll(bytes12,uint128)')"
}

test_sig_4() {
    assert_equals "0xd2ce7d65" "$(seth sig 'outboundTransfer(address,address,uint,uint,uint,bytes memory)')"
}

test_sig_5() {
    assert_equals "0xe548799c" "$(seth sig 'registerCrowdsale(address,address,uint256[8])')"
}

# tuples currently broken: https://github.com/dapphub/dapptools/issues/861
todo_sig_6() {
    assert_equals "0x5cc5e3d9" "$(seth sig 'createIncentive((address,address,uint256,uint256,address),uint256)')"
}

# ignored for now due to https://github.com/dapphub/dapptools/issues/861
todo_sig_fuzz() {
    echo
    for _ in $(seq "$FUZZ_RUNS"); do
      id=$(mod "$(uint256)" 236272)
      sig=$(curl -s "https://www.4byte.directory/api/v1/signatures/$id/")
      [[ "$sig" == '{"detail":"Not found."}' ]] && continue
      text=$(echo "$sig" | jq -r .text_signature)
      hex=$(echo "$sig" | jq -r .hex_signature)
      echo "checking ($id): $text"

      local actual
      actual=$(seth sig "$text")
      assert_equals "$hex" "$actual"
    done
}

test_keccak_1() {
  local output
  output=$(seth keccak 0xcafe)

  assert_equals "0x72318c618151a897569554720f8f1717a3da723042fb73893c064da11b308ae9" "$output"
}

test_keccak_2() {
  local output
  output=$(seth keccak 0xca:0xfe)

  assert_equals "0x72318c618151a897569554720f8f1717a3da723042fb73893c064da11b308ae9" "$output"
}

test_keccak_3() {
  local output
  output=$(seth keccak cafe)

  assert_equals "0x4c84268b4bd90011342a28648371055c58267a2a8e93a7b0bc61fe93bf186974" "$output"
}

test_hexdata_0() {
  assert_equals "0xcafe" "$(seth --to-hexdata cafe)"
}

test_hexdata_1() {
  assert_equals "0xcafe" "$(seth --to-hexdata 0xcafe)"
}

test_hexdata_2() {
  assert_equals "0xcafe" "$(seth --to-hexdata 0xCA:0xfe)"
}

test_hexdata_3() {
  assert_equals "0xcafe" "$(seth --to-hexdata 0xCA:0xfe:0x)"
}

# SETH ENS TESTS

test_namehash_1() {
    local output
    output=$(seth namehash)
    assert_equals "0x0000000000000000000000000000000000000000000000000000000000000000" "$output"
}

test_namehash_2() {
    local output
    output=$(seth namehash eth)
    assert_equals "0x93cdeb708b7545dc668eb9280176169d1c33cfd8ed6f04690a0bcc88a93fc4ae" "$output"
}

test_namehash_3() {
    local output
    output=$(seth namehash ricmoo.firefly.eth)
    assert_equals "0x0bcad17ecf260d6506c6b97768bdc2acfb6694445d27ffd3f9c1cfbee4a9bd6d" "$output"
}

test_namehash_4() {
    local output
    output=$(seth namehash seth-test.eth)
    assert_equals "0xc639cb4715c456d2cc8523ee5568222dbae3551e2a42c61a7da4db3ec28ab9e9" "$output"
}

test_namehash_5() {
    assert_equals "$(seth namehash ETH)" "$(seth namehash eth)"
}

test_namehash_6() {
    assert_equals "$(seth namehash ricmoo.firefly.eth)" "$(seth namehash RicMOO.FireFly.eTH)"
}

test_namehash_7() {
    assert_equals "$(seth namehash seth-test.eth)" "$(seth namehash sEtH-tESt.etH)"
}

# SETH 4BYTE TESTS
# seth 4byte
test_4byte() {
    assert_equals "transfer(address,uint256)" "$(seth 4byte a9059cbb | tail -n 1)"
}

# SETH FIXED POINT TESTS
# seth --from-fix
test_from_fix1() {
    assert_equals 1000000 "$(seth --from-fix 6 1)"
}

test_from_fix2() {
    assert_equals 1000000000000000000 "$(seth --from-fix 18 1)"
}

test_from_fix3() {
    assert_equals 1234500 "$(seth --from-fix 6 1.2345)"
}

test_from_fix4() {
    assert_equals 1234567890000000000 "$(seth --from-fix 18 1.23456789)"
}

# seth --to-fix
test_to_fix1() {
    assert_equals 1.000000 "$(seth --to-fix 6 1000000)"
}

test_to_fix2() {
    assert_equals 1.000000000000000000 "$(seth --to-fix 18 1000000000000000000)"
}

test_to_fix3() {
    assert_equals 1.234500 "$(seth --to-fix 6 1234500)"
}

test_to_fix4() {
    assert_equals 1.234567890000000000 "$(seth --to-fix 18 1234567890000000000)"
}

# SETH RUN-TX TESTS
test_run_tx_source_fetching() {
    export ETH_RPC_URL=$ARCHIVE_NODE_URL
    local out err
    out=$(mktemp)
    err=$(mktemp)

    # prints a message when source is not available
    seth run-tx 0xc1511d7fcc498ae8236a18a67786701e6980dcf641b72bcfd4c2a3cd45fb209c --trace 1> "$out" 2> "$err"
    assert "grep -q 'Contract source code not verified' $err" 1
    assert "grep -q 'delegatecall 0xAa1c1B3BbbB59930a4E88F87345B8C513cc56Fa6::0x526327f2' $err" 2
    assert_equals "0x188fffa3a6cd08bdcc3d5bf4add2a2c0ac5e9d94a278ea1630187b3da583a1f0" "$(cat "$out")"

    local prefiles
    prefiles=$(ls)

    # seth pulls from etherscan by default (flattened)
    seth run-tx 0x41ccbab4d7d0cd55f481df7fce449986364bf13e655dddfb30aa9b38a4340db7 --trace 1> "$out" 2> "$err"
    assert "grep -q 'UniswapV2Pair@0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C' $err" 3
    assert "grep -q 'PairCreated(UniswapV2Pair@0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C, 51691)' $err" 4
    assert_equals "0x00000000000000000000000028d2df1e3481ba90d75e14b9c02cea85b7d6fa2c" "$(cat "$out")"

    # seth does not write any files to the cwd
    assert_equals "$prefiles" "$(ls)"

    # seth pulls from etherscan by default (stdjson)
    seth run-tx 0x5da4bf1e5988cf94fd96d2c1dd3f420d2cea1aebe8d1e1c10dd9fe78a2147798 --trace 1> "$out" 2> "$err"
    assert "grep -q 'ownerOf' $err" 5
    assert "grep -q 'iFeather@0xD1edDfcc4596CC8bD0bd7495beaB9B979fc50336' $err" 6
    assert_equals "0x" "$(cat "$out")"

    # seth does not write any files to the cwd
    assert_equals "$prefiles" "$(ls)"

    # seth does not pull from etherscan if --source is passed
    seth run-tx 0x41ccbab4d7d0cd55f481df7fce449986364bf13e655dddfb30aa9b38a4340db7 --trace --source /dev/null 1> "$out" 2> "$err"
    assert "grep -q 'call 0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C::0x485cc9550000000000000000000000007fa7df4' $err" 7
    assert "grep -q 'log3(0xd3648bd0f6ba80134a33ba9275ac585d9d315f0ad8355cddefde31afa28d0e9, 0xffffffffffffffff' $err" 8
    assert_equals "0x00000000000000000000000028d2df1e3481ba90d75e14b9c02cea85b7d6fa2c" "$(cat "$out")"

    # seth does not pull from etherscan if --no-src is passed at the command line
    seth run-tx 0x41ccbab4d7d0cd55f481df7fce449986364bf13e655dddfb30aa9b38a4340db7 --trace --no-src 1> "$out" 2> "$err"
    assert "grep -q 'call 0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C::0x485cc9550000000000000000000000007fa7df4' $err" 9
    assert "grep -q 'log3(0xd3648bd0f6ba80134a33ba9275ac585d9d315f0ad8355cddefde31afa28d0e9, 0xffffffffffffffff' $err" 10
    assert_equals "0x00000000000000000000000028d2df1e3481ba90d75e14b9c02cea85b7d6fa2c" "$(cat "$out")"

    # seth does not pull from etherscan if SETH_NOSRC is set to "yes"
    SETH_NOSRC=yes seth run-tx 0x41ccbab4d7d0cd55f481df7fce449986364bf13e655dddfb30aa9b38a4340db7 --trace 1> "$out" 2> "$err"
    assert "grep -q 'call 0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C::0x485cc9550000000000000000000000007fa7df4' $err" 11
    assert "grep -q 'log3(0xd3648bd0f6ba80134a33ba9275ac585d9d315f0ad8355cddefde31afa28d0e9, 0xffffffffffffffff' $err" 12
    assert_equals "0x00000000000000000000000028d2df1e3481ba90d75e14b9c02cea85b7d6fa2c" "$(cat "$out")"

    # seth does not pull from etherscan if ETHERSCAN_API_KEY is unset
    unset ETHERSCAN_API_KEY
    seth run-tx 0x41ccbab4d7d0cd55f481df7fce449986364bf13e655dddfb30aa9b38a4340db7 --trace 1> "$out" 2> "$err"
    assert "grep -q 'call 0x28d2DF1E3481Ba90D75E14b9C02cea85b7d6FA2C::0x485cc9550000000000000000000000007fa7df4' $err" 13
    assert "grep -q 'log3(0xd3648bd0f6ba80134a33ba9275ac585d9d315f0ad8355cddefde31afa28d0e9, 0xffffffffffffffff' $err" 14
    assert_equals "0x00000000000000000000000028d2df1e3481ba90d75e14b9c02cea85b7d6fa2c" "$(cat "$out")"
}
