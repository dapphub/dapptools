{ system ? builtins.currentSystem, ... }:

let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "2dea8991d89b9f1e78d874945f78ca15f6954289";
    ref = "nixos-22.11";
    shallow = true;
  };
  ethereum-hevm = import (builtins.fetchGit {
    name = "ethereum-hevm";
    url = "https://github.com/ethereum/hevm.git";
    # TODO get from lock filr
    rev = "3a2465ddf39df868d5f4f69b219a9a584c6a60da";
    shallow = true;
  });
in

# Now return the Nixpkgs configured to use our overlay.
import nixpkgs {
  inherit system;

  overlays = [
    (import ./overlay.nix)
    (final: prev: {
      hevm = ethereum-hevm.packages.${system}.hevm;
    })
  ];
}
