{ dapphub ? import ../.. {} }:
let
  inherit (dapphub) pkgs;
  f = import ./default.nix;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
    });
  };
  drv = haskellPackages.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
