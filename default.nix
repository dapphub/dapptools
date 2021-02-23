{ system ? builtins.currentSystem , ... }:

let
  rev = "8e78c2cfbae71720869c35b6710d652bf28b37cc";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-20.09";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "178dr6bz71lbv0ljynvvkrc2p0lwqmci482brkqdw9qfx3sc1a7f";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

