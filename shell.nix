{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  buildTools = [];
  haskellPackages = pkgs.haskellPackages.override {
    overrides = (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
    });
  };
  drv = pkgs.haskell.lib.addBuildTools (
    haskellPackages.callPackage (import ./default.nix) {}
  ) buildTools;
in
  if pkgs.lib.inNixShell then drv.env else drv
