default.nix: hsevm.cabal; cabal2nix --enable-profiling . > default.nix

nix: default.nix hsevm.nix
	nix-shell -A hsevm.env hsevm.nix --command \
	  'cabal configure --enable-tests'

nix-profiling: default.nix hsevm.nix
	nix-shell -A hsevmProfiling.env hsevm.nix \
	  --command 'cabal configure --enable-profiling'

docker:; docker build -t dapphub/hsevm .

# Static binary built via Docker
hsevm-linux-x64: docker
	docker run --rm dapphub/hsevm cat /bin/hsevm > $@
	chmod +x $@
