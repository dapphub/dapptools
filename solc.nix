{ ... }:

let
  system = (sys: (import ./default.nix { inherit sys; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit solc;
  };

in {
  dapphub.linux.stable = stable linux;
  dapphub.darwin.stable = stable darwin;
}
