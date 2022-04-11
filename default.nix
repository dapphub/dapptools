{ system ? builtins.currentSystem , ... }:

let
  rev = "fd364d268852561223a5ada15caad669fd72800e";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable-22-04-11";
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "133i5fsx0gix37q4nxm1vfsl9hqbfzv458xykilqhgxmv45jmfl2";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [(import ./overlay.nix)];
  }
