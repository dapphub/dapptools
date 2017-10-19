{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v0.20171019";
      sha256 = "0y1m1g2552gnpskqx2rprsjjyk0nj3vnb6c17whk0qxgfyxxcp8d";
    });
  drv =
    pkgs.haskell.lib.justStaticExecutables
      (pkgs.haskellPackages.callPackage src {});

in
  lib.overrideDerivation drv
    (attrs: {
      postInstall = ''
        cp $out/bin/{jays,jshon}
      '';

      maintainers = [lib.maintainers.dbrock];
    })
