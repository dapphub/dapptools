{ system ? null , ... }:
let
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "8c2c14ac392e5b96a1b3d12e16ba0439689024c7";
    sha256 = "0x3b0ml7gxc9y28y4l64mx6w5582ncks0rca00ikn1pqffffvbxi";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs (
    { overlays = [(import ./overlay.nix)]; }
    // (if system != null then { inherit system; } else {})
  )

