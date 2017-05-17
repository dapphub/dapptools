default.nix: hsevm.cabal; cabal2nix --enable-profiling . > default.nix
nix: default.nix hsevm.nix
	nix-shell -A hsevm.env hsevm.nix --command 'cabal configure --enable-tests'

repl: nix; cabal repl
test: nix; cabal test

tty-ghci: nix; cabal repl hsevm-tty
tty: nix; cabal build hsevm-tty

# bench: default.nix
# 	nix-shell -A hsevmProfiling.env hsevm.nix --command \
#         'cabal configure --enable-profiling --enable-benchmarks && cabal bench'

.PHONY: test