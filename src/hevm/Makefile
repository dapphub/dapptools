all: build

default.nix: hevm.cabal
	nix-shell --command 'cabal2nix . > default.nix'
.configured: default.nix shell.nix
	nix-shell --command 'cabal configure --enable-tests'
	touch .configured
build: .configured
	nix-shell --command 'cabal build'
repl: .configured
	nix-shell --command 'cabal repl lib:hevm'
tests: .configured
	nix-shell --command 'cabal test'

PORT ?= 8001
BROWSER ?= chromium
hoogle-server:; nix-shell --run 'hoogle server --local -p $(PORT)'
