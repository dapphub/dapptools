{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v0.20171012";
      sha256 = "0qbz6lya0v5k6x9bqp5xa7vl7i1qhzyraq677yrfclmawb7r87zl";
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
