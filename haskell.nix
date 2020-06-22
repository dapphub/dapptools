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
    sbv_8_4_prepatch = self-hs.callCabal2nix "sbv" (builtins.fetchGit {
        url = "https://github.com/LeventErkok/sbv/";
        rev = "b3facdfc959e0779fb8944743c0ec81e0d214ee3";
    }) {inherit (pkgs) z3;};

  in {
    restless-git = dontCheck (import ./src/restless-git);
    wreq = pkgs.haskell.lib.doJailbreak super-hs.wreq;

    # we use a pretty bleeding edge sbv version
    sbv_8_4 = sbv_8_4_prepatch.overrideAttrs (attrs: {
      postPatch = ''
      sed -i -e 's|"z3"|"${pkgs.z3}/bin/z3"|' Data/SBV/Provers/Z3.hs
      sed -i -e 's|"cvc4"|"${pkgs.cvc4}/bin/cvc4"|' Data/SBV/Provers/CVC4.hs'';
    });


    # This package is somewhat unmaintained and doesn't compile with GHC 8.4,
    # so we have to use a GitHub fork that fixes it.
    semver-range = super-hs.semver-range.overrideAttrs (attrs: {
      src = pkgs.fetchFromGitHub {
        owner = "dmjio";
        repo = "semver-range";
        rev = "patch-1";
        sha256 = "1l20hni4v4k6alxj867z00625pa5hkf0h5sdaj1mjc237k5v78j9";
      };
      meta.broken = false;
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
