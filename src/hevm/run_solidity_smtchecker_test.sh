#!/usr/bin/sh

# Takes one Solidity file in the format of an SMTChecker test,
# runs `hevm symbolic` on it and compares results against the SMTChecker expectations.

hevm_asserts=$(./run_solidity_symbolic.sh $1)
if [ $? -eq 124 ]
then
	echo "HEVM timeout."
	exit 1
fi

grep -q 'Assertion violation found' <<< "$hevm_asserts"
hevm_violation=$?
grep -q 'Assertion violation happens' $1
smtchecker_violation=$?

if [ $hevm_violation -ne 0 ] && [ $smtchecker_violation -eq 0 ]
then
	echo "SMTChecker reports assertion violation whereas HEVM reports safe."
	exit 1
elif [ $hevm_violation -eq 0 ] && [ $smtchecker_violation -ne 0 ]
then
	echo "SMTChecker reports safe whereas HEVM reports assertion violation. Potential false positive."
	exit 1
fi

exit 0
