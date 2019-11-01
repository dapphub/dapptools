source $stdenv/setup
unpackPhase

jsonopts=--combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast

export DAPP_SRC=$src
export DAPP_OUT=out

find "$DAPP_SRC" -name '*.sol' | while read -r x; do
  dir=${x%\/*}
  dir=${dir#$DAPP_SRC}
  dir=${dir#/}
  json_file=$DAPP_OUT/$dir/${x##*/}.json
  mkdir -p "$DAPP_OUT/$dir"
  (set -x; solc $REMAPPINGS --allow-paths $DAPP_SRC $solcFlags $jsonopts "$x" >"$json_file")
  if [[ $flatten == 1 && ! $x =~ \.t(\.[a-z0-9]+)*\.sol$ ]]; then
    flat_file="$DAPP_OUT/$dir/${x##*/}.flat"
    (set -x; hevm flatten --source-file "$x" --json-file "$json_file" >"$flat_file")
    x="$flat_file"
  fi
  (set -x; solc --overwrite $REMAPPINGS --allow-paths $DAPP_SRC $solcFlags --abi --bin --bin-runtime -o "$DAPP_OUT/$dir" "$x")
done

mkdir lib
echo "$LIBSCRIPT" > setup.sh
source setup.sh
export DAPP_LIB=lib

if [ "$doCheck" == 1 ]; then
  dapp2-test-hevm
fi

mkdir -p $out/dapp/$name
cp -r $src $out/dapp/$name/src
cp -r lib $out/dapp/$name/lib
cp -r out $out/dapp/$name/out
