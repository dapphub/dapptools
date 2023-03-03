# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib, wrapped ? true, shared ? false }:

let
  stdenv = pkgs.stdenv;

in self-hs: super-hs:
  let
    dontCheck = x: y:
      pkgs.haskell.lib.dontCheck
        (self-hs.callCabal2nix x y {});

    # FIXME: hevm is broken in our current nixpkgs pin, so we pull a newer one
    # here and then use that to build hevm-0.50.0. This should be removed once
    # we migrate the main nixpkgs pin to a newer version.
    pkgs-2022-11 = import (pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "dac57a4eccf1442e8bf4030df6fcbb55883cb682";
      sha256 = "sha256-C15oAtyupmLB3coZY7qzEHXjhtUx/+77olVdqVMruAg=";
    }) { system = pkgs.system; };
    myHaskell = pkgs-2022-11.haskellPackages.override {
      overrides = self: super: {
        hevm = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
          pkg = "hevm";
          ver = "0.50.0";
          sha256 = "sha256-ju/ZuacGneQR6tJLv7gwyMj7+u8GGQ5JcYm/XXi53yI=";
        } {});
        secp256k1 = pkgs.secp256k1;
      };
    };

  in {
    restless-git = dontCheck "restless-git" (./src/restless-git);

    eth-utils = pkgs.haskell.lib.dontHaddock (
      myHaskell.callCabal2nix "eth-utils" (./src/eth-utils) {}
    );
  }
