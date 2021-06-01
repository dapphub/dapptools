{ system ? builtins.currentSystem , ... }:

let
  rev = "6933d068c5d2fcff398e802f7c4e271bbdab6705";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "027axlv5g1l5sxkcnlswhmbc8y0xbgppggg9mhmfb78x7d8anqzq";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

