{ pkgs, stdenv }:
  pkgs.lib.overrideDerivation
    (pkgs.haskell.lib.justStaticExecutables
      (pkgs.haskellPackages.callPackage ./cabal/jays.nix {}))
  (attrs: {
    src = pkgs.fetchFromGitHub {
      owner = "mbrock";
      repo = "jays";
      rev = "v1.20171020";
      sha256 = "1d83zvv55gm417pxzzbfl2s2f80mia0fxidqs0lahfppb6zb09fp";
    };
    postInstall = "cp $out/bin/{jays,jshon}";
    maintainers = [pkgs.lib.maintainers.dbrock];
  })
