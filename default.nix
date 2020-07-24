{ system ? builtins.currentSystem , ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "release-20.03";
    rev = "5272327b81ed355bbed5659b8d303cf2979b6953";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

