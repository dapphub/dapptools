{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v0.20171014";
      sha256 = "1k7ybqcws3rgnkkmnp4kd5b7vcmmvysfkc20acdaxyd2yrs8c2i4";
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
