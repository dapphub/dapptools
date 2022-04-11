{ dapphub ? import ../.. {} }:
let
  inherit (dapphub) pkgs;
in
  pkgs.haskellPackages.shellFor {
    packages = p: [
      p.smt-hs
    ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
    ];
    withHoogle = true;
  }
