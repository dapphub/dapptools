{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";

in rec {
  dapphub.ethos-iso = linux.pkgs.ethos-iso;
  dapphub.ethos-iso-hidpi = linux.pkgs.ethos-iso-hidpi;
}
