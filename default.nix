{ system ? null , ... }:
let
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "18.09";
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs (
    { overlays = [(import ./overlay.nix)]; }
    // (if system != null then { inherit system; } else {})
  )

