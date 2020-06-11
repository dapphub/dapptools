source $stdenv/setup
unpackPhase

jsonopts=--combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast,metadata

export DAPP_SRC=$src
export DAPP_OUT=out

mkdir -p "$DAPP_OUT"
mapfile -t files < <(find "${DAPP_SRC}" -name '*.sol')
(set -x; solc "$REMAPPINGS" "$jsonopts" "$solcFlags" /=/ "${files[@]}" > "${DAPP_JSON}")

mkdir lib
echo "$LIBSCRIPT" > setup.sh
source setup.sh
export DAPP_LIB=lib

if [ "$doCheck" == 1 ]; then
  dapp2-test-hevm
fi

if [ "$extract" == 1 ]; then
  mapfile -t contracts < <(<"$DAPP_JSON" jq '.contracts|keys[]' -r | sort -u -t: -k2 | sort)
  data=$(<"$DAPP_JSON" jq '.contracts' -r)
  count=1
  total=${#contracts[@]}
  for path in "${contracts[@]}"; do
    info -ne "Extracting build data... [$count/$total]\\r"
    ((count++))
    name="${path#*:}"
    contract=$(echo "$data" | jq '.[''"'"$path"'"'']')
    echo "$contract" | jq '.["abi"]' -r > "$DAPP_OUT/$name.abi"
    echo "$contract" | jq '.["bin"]' -r > "$DAPP_OUT/$name.bin"
    echo "$contract" | jq '.["bin-runtime"]' -r > "$DAPP_OUT/$name.bin-runtime"
    echo "$contract" | jq '.["metadata"]' -r > "$DAPP_OUT/$name.metadata"
  done
  echo
  info "warning: --extract is slow and may have filename collisions. All build data can be found in $DAPP_JSON.\\n"
fi

mkdir -p "$out/dapp/$name"
cp -r "$src" "$out/dapp/$name/src"
cp -r lib "$out/dapp/$name/lib"
cp -r out "$out/dapp/$name/out"
