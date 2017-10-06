{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.9.5";
      sha256 = "05w53gvbsdjp1xj51855f87bpfljzs4jmdkddc3gi3mpcq5y8bs1";
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
