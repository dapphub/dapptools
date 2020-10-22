{ system ? builtins.currentSystem , ... }:

let
  rev = "5272327b81ed355bbed5659b8d303cf2979b6953";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-20.03";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

