{ system ? builtins.currentSystem , ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "release-19.03";
    rev = "f1707d8875276cfa110139435a7e8998b4c2a4fd";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

