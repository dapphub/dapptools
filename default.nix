{ system ? builtins.currentSystem , ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
#    ref = "release-20.03";
    rev = "6eae50cca8014a2eb773d73bbe10552c60841217";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

