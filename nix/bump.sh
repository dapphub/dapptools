#!/usr/bin/env bash
set -e

mkdir -p overlay/upstream/{master,stable}
echo '{}' >bump.json

pkgs=(
  dapphub/dapp
  mbrock/jays
  dapphub/hevm
  dapphub/seth
  dapphub/ethsign
  dapphub/token
  lessrest/restless-git
  dapphub/libethjet
  dapphub/libethjet-haskell
  mbrock/symbex
  makerdao/setzer
  makerdao/dai-cli
  mbrock/oasis-orders
)

printf "%s\n" "${pkgs[@]}" | xargs -L 1 --max-procs=8 ./bump-one.sh

jq --sort-keys . < ./bump.json > overlay/versions.json
rm bump.json
