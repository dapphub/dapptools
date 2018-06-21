{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
    });
  };
  drv = haskellPackages.callPackage (import ./default.nix) {};
in
  if pkgs.lib.inNixShell then drv.env else drv
