#!/usr/bin/env bats

@test "dapp without arguments prints usage" {
  dapp
  [ "${lines[0]}" = "Usage: dapp <command> [args]" ]
}
