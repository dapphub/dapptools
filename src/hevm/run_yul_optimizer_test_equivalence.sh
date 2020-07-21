#!/usr/bin/sh

# Takes one file which follows the Solidity Yul optimizer unit tests format,
# extracts both the nonoptimized and the optimized versions, and checks equivalence.

nonopt_src=$(mktemp)
sed '0,/^\/\/ step:/d' $1 | sed -e 's!\/\/!!' > $nonopt_src
./run_yul_equivalence.sh $1 $nonopt_src
rm $nonopt_src
