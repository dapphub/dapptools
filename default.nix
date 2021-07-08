{ system ? builtins.currentSystem , ... }:

let
  rev = "3d64b6067f1c457f889e91c59ced273be1d1e4f1";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-21.05";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "0wgmzq2h82wxqh6zkxxxw77rdk75m48rshmjfl3w35pv498wfcwz";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
