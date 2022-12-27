{ dapphub ? import ../.. {} }:
let
  inherit (dapphub) pkgs;


  drv = pkgs.haskellPackages.shellFor {
    packages = p: [
      p.hevm
    ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
    ];
    withHoogle = true;
  };
in
  if pkgs.lib.inNixShell
  then drv.overrideAttrs (_: {
    LD_LIBRARY_PATH = "${pkgs.secp256k1}/lib:${pkgs.libff}/lib";
  })
  else drv
