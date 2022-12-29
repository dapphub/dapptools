{ system ? builtins.currentSystem , ... }:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-release-21.05";
    url = "https://github.com/nixos/nixpkgs/tarball/${lock.nodes.nixpkgs_2.locked.rev}";
    sha256 = lock.nodes.nixpkgs_2.locked.narHash;
  };
  hevm = import (builtins.fetchTarball {
    name = "ethereum-hevm";
    url = "https://github.com/ethereum/hevm/tarball/${lock.nodes.ethereum-hevm.locked.rev}";
    sha256 = lock.nodes.ethereum-hevm.locked.narHash;
  });
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs {
    inherit system;

    overlays = [ (import ./overlay.nix) ];
  }
