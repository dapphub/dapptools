default.nix: hsevm.cabal; cabal2nix --enable-profiling . > default.nix
nix: default.nix hsevm.nix
	nix-shell -A hsevm.env hsevm.nix --command 'cabal configure --enable-tests'

repl: nix; cabal repl
test: nix; cabal test

profile: default.nix hsevm.nix
	nix-shell -A hsevmProfiling.env hsevm.nix \
	  --command 'cabal configure --enable-profiling'

# bench: default.nix
# 	nix-shell -A hsevmProfiling.env hsevm.nix --command \
#         'cabal configure --enable-profiling --enable-benchmarks && cabal bench'

.PHONY: test