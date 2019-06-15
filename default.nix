{ system ? builtins.currentSystem , ... }:

let
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-${version}";
    # pin the current release-18.09 commit
    url = "https://github.com/nixos/nixpkgs/archive/185ab27b8a2ff2c7188bc29d056e46b25dd56218.tar.gz";
    sha256 = "0bflmi7w3gas9q8wwwwbnz79nkdmiv2c1bpfc3xyplwy8npayxh2";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

