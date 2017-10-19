{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.6";
      sha256 = "0lv5bwvxkccyqy00fdz1rwkgwsxp91z09prdagb48d53hgbkl3lx";
    });
  drv =
    pkgs.haskell.lib.justStaticExecutables
      (pkgs.haskellPackages.callPackage src {});

in
  lib.overrideDerivation drv
    (attrs: {
      postInstall = ''
        wrapProgram $out/bin/hevm \
           --add-flags '+RTS -N$((`${pkgs.coreutils}/bin/nproc` - 1)) -RTS' \
           --suffix PATH : "${lib.makeBinPath [pkgs.bash pkgs.coreutils pkgs.git]}"
      '';

      enableSeparateDataOutput = true;
      maintainers              = [stdenv.lib.maintainers.dbrock];
      buildInputs              = attrs.buildInputs ++ [pkgs.solc];
      nativeBuildInputs        = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
    })
