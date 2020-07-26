all: build

.configured: shell.nix
	nix-shell --command 'cabal new-configure --enable-tests' --pure
	touch .configured
build: .configured
	nix-shell --command 'cabal new-build' --pure
repl: .configured
	nix-shell --command 'cabal new-repl lib:hevm' --pure
tests: .configured
	nix-shell --command 'cabal new-test' --pure

PORT ?= 8001
BROWSER ?= chromium
hoogle-server:; nix-shell --run 'hoogle server --local -p $(PORT)'

.PHONY: all build repl
