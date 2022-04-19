{ system ? builtins.currentSystem , ... }:

let
  rev = "bd4dffcdb7c577d74745bd1eff6230172bd176d5";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable-2022-04-17";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "18zacrykj2k5x42d0grr7g1y7xhy5ppq7j0gm3lrghwflyrdkslj";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
