source $stdenv/setup
unpackPhase

jsonopts=--combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast

export DAPP_SRC=$src/src
export DAPP_OUT=out

find "$DAPP_SRC" -name '*.sol' | while read -r x; do
  dir=${x%\/*}
  dir=${dir#$DAPP_SRC}
  dir=${dir#/}
  mkdir -p "$DAPP_OUT/$dir"
  (set -x; solc --overwrite $REMAPPINGS --abi --bin --bin-runtime = -o "$DAPP_OUT/$dir" "$x")
  json_file=$DAPP_OUT/$dir/${x##*/}.json
  (set -x; solc $REMAPPINGS $jsonopts = "$x" >"$json_file")
done

mkdir lib
echo "$LIBSCRIPT" > setup.sh
source setup.sh
dapp test-hevm

mkdir -p $out/{src,lib,out}
cp -r $src/src $out
cp -r out $out
