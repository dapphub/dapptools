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
      ff = pkgs.libff;
    }
  ) [pkgs.git pkgs.cabal-install pkgs.cabal2nix pkgs.solc];

in
  if pkgs.lib.inNixShell
  then drv.env.overrideAttrs (_: {
    LD_LIBRARY_PATH = "${pkgs.secp256k1}/lib:${pkgs.libff}/lib";
  })
  else drv
