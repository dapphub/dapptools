{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.8";
      sha256 = "0wfy32c28fkf1g4lm4rsgw1z8i3xhg82c1ccsb5ipd4lm0bad8i1";
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
