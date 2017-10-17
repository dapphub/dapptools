{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.5";
      sha256 = "1dpq0a691pwlwakgws60wi01apvlq161j0jh59fh7jibldanhspp";
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
