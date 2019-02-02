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
  ) [pkgs.git pkgs.cabal-install];

in
  if pkgs.lib.inNixShell
  then drv.env.overrideAttrs (_: {
    LD_LIBRARY_PATH = "${pkgs.secp256k1}/lib";
  })
  else drv
