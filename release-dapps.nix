{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";

  legacyDappPackage = (import ./overlay/build-dapp.nix {
    inherit (linux) pkgs;
  }).dappPackage;

in rec {
  dappsys = linux.pkgs.dappsys;
  dappsys-legacy = linux.pkgs.dappsys-legacy;

  maker.dai-v1 = legacyDappPackage {
    name = "dai-v1";
    src = linux.pkgs.fetchFromGitHub {
      repo = "sai";
      owner = "makerdao";
      rev = "dai-v1";
      sha256 = "0ikwajpc4mj67p2q5bbppj5r9sjixfcv8vrh21w8mld23ngc2fwi";
    };
    dependencies = with dappsys-legacy; [
      ds-chief ds-guard ds-roles ds-spell ds-test ds-thing ds-token ds-value
    ];
  };
}
