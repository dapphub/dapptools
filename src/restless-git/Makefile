all: default.nix nix; nix-shell --command 'cabal build'
default.nix: restless-git.cabal; cabal2nix . > default.nix
nix: default.nix shell.nix
	nix-shell --command \
	  'cabal configure --enable-tests'

.PHONY: all nix
