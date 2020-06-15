{ system ? builtins.currentSystem , ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "release-19.09";
    rev = "54e89941c303687a6392a3264dbe1540fe3381d8";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

