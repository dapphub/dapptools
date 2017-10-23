{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.9";
      sha256 = "00yvajyfkkhz1df7rd5ib343pdfid4qf3c61pph22jb2hhzas96b";
    });
  drv =
    pkgs.haskell.lib.justStaticExecutables
      (pkgs.haskellPackages.callPackage src {});

in
  lib.overrideDerivation drv
    (attrs: {
      postInstall = ''
        wrapProgram $out/bin/hevm \
           --suffix PATH : "${lib.makeBinPath [pkgs.bash pkgs.coreutils pkgs.git]}"
      '';

      enableSeparateDataOutput = true;
      maintainers              = [stdenv.lib.maintainers.dbrock];
      buildInputs              = attrs.buildInputs ++ [pkgs.solc];
      nativeBuildInputs        = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
    })
