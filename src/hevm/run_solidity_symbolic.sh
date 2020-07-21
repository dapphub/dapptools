#!/usr/bin/sh

# Takes one Solidity file, compiles it to bytecode and runs
# `hevm symbolic` on it.

compile_solidity()
{
	solc --bin-runtime $1 2> /dev/null | awk 'f{print;f=0} /Binary of the runtime part:/{f=1}'
}

echo "Checking for assertion violations in $1"

timeout 10s ./hevm symbolic --code $(compile_solidity $1) --solver cvc4
if [[ $? == 124 ]]
then
	echo "HEVM timeout."
	exit 124
fi

exit 0
