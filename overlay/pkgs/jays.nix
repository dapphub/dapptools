{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v1.20171020";
      sha256 = "1d83zvv55gm417pxzzbfl2s2f80mia0fxidqs0lahfppb6zb09fp";
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
