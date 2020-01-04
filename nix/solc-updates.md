# solc updates

1. update `dapphub/nixpkgs:master` to `nixos/nixpkgs:master`
1. create a new branch (`solc-X.Y.Z`) from `dapphub/nixpkgs:master`
    1. in `pkgs/development/compilers/solc/default.nix`
        1. set `version` to `X.Y.Z`
        1. update `rev` and `sha256` based on the output from `nix-prefetch-git git@github.com:ethereum/solidity.git vX.Y.Z`
    1. check the build by running `nix-build -A solc` from the repo root
    1. tweak as needed to get the build passing
    1. install the new version of solc into your local env (`nix-env -f . -iA solc`) and run a few
       tests to make sure everything is OK.
    1. commit the changes
1. create a new branch (`solc-X.Y.Z`) in `dapphub/dapptools`
    1. in `nix/solc-versions.nix`
        1. add a new `solc_X.Y.Z` in the `unreleased` section
        1. update `rev` and `sha256` based on the output from `nix-prefetch-git
           git@github.com:dapphub/nixpkgs refs/heads/solc-X.Y.Z`
    1. bump the version number in `dapp---version`, `src/dapp/default.nix` and the changelog
    1. commit the changes
1. open a pr from `dapphub/dapptools:solc-X.Y.Z` to `dapphub/dapptools:master`
1. once merged tag the commit with the new version number
1. open a pr from `dapphub/nixpkgs:solc-X.Y.Z` to `nixos/nixpkgs:master`
1. once merged make a new PR in `dapphub/dapptools` moving the `solc_X.Y.Z` from the `unreleased` to
   the `common` section in `solc-versions.nix`
