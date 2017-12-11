{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
#  profiling = pkgs.haskellPackages.override {
#    overrides = self: super: {
#      mkDerivation = args: super.mkDerivation (args // {
#        enableLibraryProfiling = true;
#      });
#    };
#  };

  restless-git = import (pkgs.fetchFromGitHub {
    owner = "lessrest";
    repo = "restless-git";
    rev = "v0.5.0";
    sha256 = "0l2n556fzgkgnw7rhwfsj7438qyid8y9sghlhd3is154dspg0p9v";
  });

  haskellPackages = pkgs.haskellPackages.override {
    overrides = (self: super: {
      ghc =
        super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages =
        self.ghc.withPackages;
      # restless-git =
      #   with pkgs.haskell.lib;
      #     dontCheck (self.callPackage restless-git {});
      megaparsec = super.megaparsec_6_2_0;
    });
  };

  drv = pkgs.haskell.lib.addBuildTool (
    pkgs.haskellPackages.callPackage (import ./default.nix) {}
  ) [pkgs.git];

in if pkgs.lib.inNixShell then drv.env else drv
