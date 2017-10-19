{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.7";
      sha256 = "0d3ynjixjddm5vq07l25bwixy8a6b4r24jrhc9msk427aqv9iim8";
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
