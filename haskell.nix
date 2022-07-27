# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;

in self-hs: super-hs:
  let
    dontCheck = x: y:
      pkgs.haskell.lib.dontCheck
        (self-hs.callCabal2nix x y {});

  in {
    restless-git = dontCheck "restless-git" (./src/restless-git);

    hevm = pkgs.haskell.lib.dontHaddock ((
      self-hs.callCabal2nix "hevm" (./src/hevm) {
        # Haskell libs with the same names as C libs...
        # Depend on the C libs, not the Haskell libs.
        # These are system deps, not Cabal deps.
        inherit (pkgs) secp256k1;
      }
    ).overrideAttrs (attrs: {
      enableSeparateDataOutput = true;
      buildInputs = attrs.buildInputs ++ (with pkgs; [solc z3 cvc4]);
      nativeBuildInputs = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
      configureFlags = attrs.configureFlags ++ [
          "--ghc-option=-O2"
      ];
    }));
  }
