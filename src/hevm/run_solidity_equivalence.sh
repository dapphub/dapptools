#!/usr/bin/sh

# Takes one Solidity file, compiles it via Yul with and without optimization,
# and checks equivalence for the two versions.

TMPDIR=$(mktemp -d)

compile_solidity()
{
	solc --$2 $1 2> /dev/null | sed -e '1,8d'
}

echo "Checking optimization equivalence for $1"

compile_solidity $1 ir > "$TMPDIR/nonopt_sol.yul"
compile_solidity $1 ir-optimized > "$TMPDIR/opt_sol.yul"

./run_yul_equivalence.sh "$TMPDIR/nonopt_sol.yul" "$TMPDIR/opt_sol.yul"

rm -rf "$TMPDIR"
exit 0
