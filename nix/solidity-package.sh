source $stdenv/setup
unpackPhase

jsonopts=--combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast,metadata

export DAPP_SRC=$src
export DAPP_OUT=out

mkdir -p "$DAPP_OUT"
mapfile -t files < <(find "${DAPP_SRC}" -name '*.sol')
json_file="$DAPP_OUT/dapp.sol.json"
(set -x; solc "$REMAPPINGS" "$jsonopts" "$solcFlags" /=/ "${files[@]}" > "$json_file")

mkdir lib
echo "$LIBSCRIPT" > setup.sh
source setup.sh
export DAPP_LIB=lib

if [ "$doCheck" == 1 ]; then
  dapp2-test-hevm
fi

if [ "$extract" == 1 ]; then
  mapfile -t contracts < <(<"$json_file" jq '.contracts|keys[]' -r | sort -u -t: -k2 | sort)
  data=$(<"$json_file" jq '.contracts' -r)
  total=${#contracts[@]}
  echo "Extracting build data... [Total: $total]"
  for path in "${contracts[@]}"; do
    fileName="${path#*:}"
    contract=$(echo "$data" | jq '.[''"'"$path"'"'']')
    echo "$contract" | jq '.["abi"]' -r > "$DAPP_OUT/$fileName.abi"
    echo "$contract" | jq '.["bin"]' -r > "$DAPP_OUT/$fileName.bin"
    echo "$contract" | jq '.["bin-runtime"]' -r > "$DAPP_OUT/$fileName.bin-runtime"
    echo "$contract" | jq '.["metadata"]' -r > "$DAPP_OUT/$fileName.metadata"
  done
fi

mkdir -p "$out/dapp/$fileName"
cp -r "$src" "$out/dapp/$fileName/src"
cp -r lib "$out/dapp/$fileName/lib"
cp -r out "$out/dapp/$fileName/out"
