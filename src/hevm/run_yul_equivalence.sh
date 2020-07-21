#!/usr/bin/sh

# Takes two Yul files, compiles them to EVM bytecode and checks equivalence.

compile()
{
	solc --yul --yul-dialect evm $1 2> /dev/null | awk 'f{print;f=0} /Binary representation:/{f=1}'
}

a_bin=$(compile $1)
if [ -z "$a_bin" ]
then
	echo "Could not compile first Yul source."
	exit 1
fi

b_bin=$(compile $2)
if [ -z "$b_bin" ]
then
	echo "Could not compile second Yul source."
	exit 1
fi

if [[ "$a_bin" == "$b_bin" ]]
then
	echo "Bytecodes are the same."
	exit 1
fi

echo "Checking bytecode equivalence: $a_bin vs $b_bin"
timeout 10s ./hevm equivalence --code-a $a_bin --code-b $b_bin --smttimeout 1000
if [[ $? == 124 ]]
then
	echo "HEVM timeout."
fi

exit 0
