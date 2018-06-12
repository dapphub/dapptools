all: default.nix nix; cabal build
default.nix: restless-git.cabal; cabal2nix . > default.nix
nix: default.nix shell.nix
	nix-shell --command \
	  'cabal configure --enable-tests'
