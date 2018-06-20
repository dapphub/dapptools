{ system ? null , ... }:
let
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4b649a99d8461c980e7028a693387dc48033c1f7";
    sha256 = "0iy2gllj457052wkp20baigb2bnal9nhyai0z9hvjr3x25ngck4y";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs (
    { overlays = [(import ./nix/overlay.nix)]; }
    // (if system != null then { inherit system; } else {})
  )

