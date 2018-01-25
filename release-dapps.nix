{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";

in rec {
  dappsys = linux.pkgs.dappsys;
}
