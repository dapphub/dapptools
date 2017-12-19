{ bashScript, coreutils, utillinux }:

bashScript {
  name = "dapp-which";
  version = "0";
  deps = [coreutils utillinux];
  text = ''
    declare -A table

    table[mkr-2016-03]=0xC66eA802717bFb9833400264Dd12c2bCeAa34a6d
    table[mkr-2017-11]=0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2
    table[weth-2016-06]=0xECF8F87f810EcF450940c9f60066b4a7a501d6A7
    table[weth-2017-12]=0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2
    table[mkr-redeemer-2017-12]=0x642AE78FAfBB8032Da552D619aD43F1D81E4DD7C
    table[oasis-2017-09]=0x3Aa927a97594c3ab7d7bf0d47C71c3877D1DE4A1
    table[oasis-2017-12]=0x14FBCA95be7e99C15Cc2996c6C9d841e54B79425
    table[sai-2017-07]=0x59aDCF176ED2f6788A41B8eA4c4904518e62B6A4
    table[dai-2017-12]=0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359

    if [ "$#" -eq 0 ]; then
      for k in "''${!table[@]}"; do
        echo "$k" "''${table[$k]}"
      done | sort | column -t
    elif [ "$#" -eq 1 ]; then
      if [ ''${table[$1]+_} ]; then
        echo "''${table[$1]}"
      else
        for k in "''${!table[@]}"; do
          if [ "''${table[$k],,}" == "''${1,,}" ]; then
            echo "$k"
            exit
          fi
        done
        echo >&2 "dapp-which: don't know $1"
      fi
    else
      echo >&2 "usage: dapp-which [CONTRACT-ID | ADDRESS]"
    fi
  '';
}
