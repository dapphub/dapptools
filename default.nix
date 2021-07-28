{ system ? builtins.currentSystem , ... }:

let
  rev = "aa576357673d609e618d87db43210e49d4bb1789";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-21.05";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "1868s3mp0lwg1jpxsgmgijzddr90bjkncf6k6zhdjqihf0i1n2np";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
