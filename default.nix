{ system ? builtins.currentSystem , ... }:

let
  rev = "d17a56d90ecbd1b8fc908d49598fb854ef188461";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-2022.05-2022-06-20";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "04a9ry0kk4qzy622mk3izh4azdp7plf1847mfp3zkn9gk0innswg";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
