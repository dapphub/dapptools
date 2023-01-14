{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit dapp;
    inherit ethsign;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit qrtx-term;
    inherit qrtx;
    inherit seth;
    inherit token;
    inherit solc-versions;

    inherit dapp-tests;
  };

in {
  dapphub.linux.stable = stable linux;
  dapphub.darwin.stable = stable darwin;
}
