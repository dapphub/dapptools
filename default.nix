{ system ? builtins.currentSystem , ... }:

let
  rev = "d25ea6a0d2a847fb52131da546f2a866656fbafa";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-21.05";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "0y2h71pvzrhc2svib4lwjl04hrzy8901ravvlyxlqdbal8hy3838";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }

