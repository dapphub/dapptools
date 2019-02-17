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
    wreq = pkgs.haskell.lib.doJailbreak super-hs.wreq;

    # This package is somewhat unmaintained and doesn't compile with GHC 8.4,
    # so we have to use a GitHub fork that fixes it.
    semver-range = super-hs.semver-range.overrideAttrs (attrs: {
      src = pkgs.fetchFromGitHub {
        owner = "dmjio";
        repo = "semver-range";
        rev = "patch-1";
        sha256 = "1l20hni4v4k6alxj867z00625pa5hkf0h5sdaj1mjc237k5v78j9";
      };
    });

    hevm = pkgs.haskell.lib.dontHaddock ((
      self-hs.callPackage (import ./src/hevm) {
        # Haskell libs with the same names as C libs...
        # Depend on the C libs, not the Haskell libs.
        # These are system deps, not Cabal deps.
        inherit (pkgs) secp256k1;

        ff = pkgs.libff;
      }
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
