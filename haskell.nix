# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib, wrapped ? true, shared ? false }:

self-hs: super-hs:
  let
    dontCheck = x: y:
      pkgs.haskell.lib.dontCheck
        (self-hs.callCabal2nix x y {});

  in {
    restless-git = dontCheck "restless-git" (./src/restless-git);

    eth-utils = pkgs.haskell.lib.dontHaddock (
      self-hs.callCabal2nix "eth-utils" (./src/eth-utils) {}
    );
  }
