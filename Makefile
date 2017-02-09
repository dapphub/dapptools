default.nix: hsevm.cabal; cabal2nix --enable-profiling . > default.nix
repl: default.nix
	nix-shell -A hsevm.env hsevm.nix --command \
        'cabal configure --enable-tests && cabal repl'
test: default.nix
	nix-shell -A hsevm.env hsevm.nix --command \
        'cabal test'
bench: default.nix
	nix-shell -A hsevmProfiling.env hsevm.nix --command \
        'cabal configure --enable-profiling --enable-benchmarks && cabal bench'
tty-ghci: default.nix
	nix-shell -A hsevmProfiling.env hsevm.nix --command \
        'cabal configure --enable-profiling && cabal repl hsevm-tty'
tty: default.nix
	nix-shell -A hsevmProfiling.env hsevm.nix --command \
        'cabal configure --enable-profiling && cabal build hsevm-tty'

.PHONY: test example