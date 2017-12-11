all: conf; nix-shell --command "cabal build && cabal test"
conf: default.nix shell.nix; nix-shell --command "cabal configure --enable-tests"
default.nix: *.cabal; cabal2nix . > default.nix
