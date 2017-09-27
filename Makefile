all: default.nix nix; cabal build

default.nix: hevm.cabal
	cabal2nix . > default.nix
	sed -i 's/brick/brick_0_24_2/' default.nix

nix: default.nix shell.nix
	nix-shell --command 'cabal configure --enable-tests'

nix-profiling: default.nix hevm.nix
	nix-shell -A hevmProfiling.env hevm.nix \
	  --command 'cabal configure --enable-profiling'

docker:; docker build -t dapphub/hevm .

# Static binary built via Docker
hevm-linux-x64: docker
	docker run --rm dapphub/hevm cat /bin/hevm > $@
	chmod +x $@

PORT ?= 8001
BROWSER ?= chromium
hoogle-server:; nix-shell --run 'hoogle server --local -p $(PORT)'
