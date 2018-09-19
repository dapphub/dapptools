all: default.nix nix; nix-shell --command 'cabal build'

default.nix: hevm.cabal
	cabal2nix . > default.nix

nix: default.nix shell.nix
	nix-shell --command 'cabal configure --enable-tests'

docker:; docker build -t dapphub/hevm .

ghcid:
	nix run dapphub.haskellPackages.ghcid --command ghcid --command='cabal repl lib:hevm'

# Static binary built via Docker
hevm-linux-x64: docker
	docker run --rm dapphub/hevm cat /bin/hevm > $@
	chmod +x $@

PORT ?= 8001
BROWSER ?= chromium
hoogle-server:; nix-shell --run 'hoogle server --local -p $(PORT)'
