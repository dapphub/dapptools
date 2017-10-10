{ pkgs }:

let
  inherit (pkgs) stdenv lib;
  src =
    import (pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10";
      sha256 = "03c0q8fmw9kwz6sx5s52vmvszh3i30yzyp3y9fh0vhajbncir0zg";
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
