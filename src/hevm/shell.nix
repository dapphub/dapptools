{ dapphub ? import ../.. {} }:
let
  inherit (dapphub) pkgs;

  drv = pkgs.haskellPackages.shellFor {
    packages = p: [
      p.hevm
    ];
    buildInputs = with pkgs.haskellPackages; with pkgs; [
      ghci
      ghcid
      cabal-install
      haskell-language-server
      bc coreutils curl ethsign git gnused nix jq hevm jshon nodejs tre perl solc
      gnugrep
    ];
    withHoogle = true;
  };
in
  if pkgs.lib.inNixShell
  then drv.overrideAttrs (_: {
    LD_LIBRARY_PATH = "${pkgs.secp256k1}/lib:${pkgs.libff}/lib";
  })
  else drv
