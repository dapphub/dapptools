{ system ? builtins.currentSystem , ... }:

let
  rev = "c777cdf5c564015d5f63b09cc93bef4178b19b01";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable-2022-05-06";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "0r2xhflcy5agaz4a3b8pxiyiwh32s1kl3swv73flnj1x3v69s8bm";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
