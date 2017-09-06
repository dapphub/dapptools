all: default.nix nix; cabal build
default.nix: restless-git.cabal; cabal2nix . > default.nix
nix: default.nix restless-git.nix
	nix-shell -A restless-git.env restless-git.nix --command \
	  'cabal configure --enable-tests'
