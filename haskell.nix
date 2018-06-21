# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib }:

let
  stdenv = pkgs.stdenv;

in self-hs: super-hs:
  let
    dontCheck = x:
      pkgs.haskell.lib.dontCheck
        (self-hs.callPackage x {});
    hevmBinPath = lib.makeBinPath (with pkgs; [bash coreutils git]);
  in {
    restless-git = dontCheck (import ./src/restless-git);
    symbex = dontCheck (import ./submodules/symbex);
    ethjet = self-hs.callPackage (import ./src/libethjet-haskell) {
      # Haskell libs with the same names as C libs...
      # Depend on the C libs, not the Haskell libs.
      # These are system deps, not Cabal deps.
      inherit (pkgs) secp256k1 ethjet;
    };

    wreq = pkgs.haskell.lib.doJailbreak super-hs.wreq;

    hevm = pkgs.haskell.lib.dontHaddock ((
      self-hs.callPackage (import ./src/hevm) {}
    ).overrideAttrs (attrs: {
      postInstall = ''
        wrapProgram $out/bin/hevm --suffix PATH \
          : "${lib.makeBinPath (with pkgs; [bash coreutils git])}"
      '';

      enableSeparateDataOutput = true;
      buildInputs = attrs.buildInputs ++ [pkgs.solc];
      nativeBuildInputs = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
    }));
  }
