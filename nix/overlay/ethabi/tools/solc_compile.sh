#!/bin/bash

cd res

for sol in *.sol; do
	solc --abi -o . --overwrite "$sol"
done

for abi in *.abi; do
	python -m json.tool "$abi" > tmp
	cat tmp > "$abi"
done

rm tmp
