{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v0.20171013";
      sha256 = "1crscsiqmyvx0nvf32hi7zr4vngr1ddglii5dizriqx0kwxbracz";
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
