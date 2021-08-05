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
      postInstall =
        if wrapped
        then
          ''
            wrapProgram $out/bin/hevm --prefix PATH \
              : "${lib.makeBinPath (with pkgs; [bash coreutils git solc])}"
          ''
        else "";

      enableSeparateDataOutput = true;
      buildInputs = attrs.buildInputs ++ [pkgs.solc] ++ (if wrapped then [] else [pkgs.z3 pkgs.cvc4]);
      nativeBuildInputs = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
      configureFlags = attrs.configureFlags ++ [
          "--ghc-option=-O2"
      ] ++
      (if stdenv.isDarwin then [] else
        if shared then [] else [
          "--enable-executable-static"
          "--extra-lib-dirs=${pkgs.gmp.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.glibc.static}/lib"
          "--extra-lib-dirs=${pkgs.libff.override { enableStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.ncurses.override {enableStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      ]);
    }));
  }
