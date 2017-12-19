{ lib, bashScript, coreutils, gawk }:

let contracts = import ../known-contracts.nix;

in bashScript {
  name = "dapp-which";
  version = "0";
  deps = [coreutils gawk];
  text = ''
    declare -A table

    ${builtins.concatStringsSep "\n"
        (lib.mapAttrsToList (k: v: "table[${k}]=\"${v}\"") contracts)}

    if [ "$#" -eq 0 ]; then
      for k in "''${!table[@]}"; do
        echo "$k" "''${table[$k]}"
      done | sort | awk '{ printf("%32s  %s\n", $1, $2) }'
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
