{ dapphub ? import ../.. {} }:
let
  inherit (dapphub) pkgs;

  haskellPackages = pkgs.haskellPackages.extend (
    self: super: {
      ghc =
        super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages =
        self.ghc.withPackages;
    }
  );

  drv = pkgs.haskell.lib.addBuildTool (
    haskellPackages.callPackage (import ./default.nix) {
      inherit (pkgs) secp256k1;
    }
  ) [pkgs.git];

in if pkgs.lib.inNixShell then drv.env else drv
