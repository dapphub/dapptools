install:; nix-env -f . -iA dapp seth hevm
build-linux:; nix-build --no-out-link -A dapphub.linux.stable release.nix
build-darwin:; nix-build --no-out-link -A dapphub.darwin.stable release.nix
