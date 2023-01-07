{ system ? builtins.currentSystem , ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "2dea8991d89b9f1e78d874945f78ca15f6954289";
    ref = "nixos-22.11";
    shallow = true;
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
