{ system ? builtins.currentSystem, ... }:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = lock.nodes.nixpkgs_2.locked.rev;
    shallow = true;
  };
  ethereum-hevm = import (builtins.fetchGit {
    url = "https://github.com/ethereum/hevm.git";
    rev = lock.nodes.ethereum-hevm.locked.rev;
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
