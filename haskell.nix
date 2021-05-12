# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib, wrapped ? true }:

let
  stdenv = pkgs.stdenv;

in self-hs: super-hs:
  let
    dontCheck = x: y:
      pkgs.haskell.lib.dontCheck
        (self-hs.callCabal2nix x y {});

    sbv_prepatch = pkgs.haskell.lib.dontCheck (self-hs.callCabal2nix "sbv"
      (builtins.fetchGit {
        url = "https://github.com/LeventErkok/sbv";
        rev = "b64905e2698c320ac14ffbad53325d33081839fb";
      })
      {inherit (pkgs) z3;});

  in {
    restless-git = dontCheck "restless-git" (./src/restless-git);
    wreq = pkgs.haskell.lib.doJailbreak super-hs.wreq;

    sbv = sbv_prepatch.overrideAttrs (attrs: {
      postPatch =
        if wrapped
        then
          ''
             sed -i -e 's|"z3"|"${pkgs.z3}/bin/z3"|' Data/SBV/Provers/Z3.hs
             sed -i -e 's|"cvc4"|"${pkgs.cvc4}/bin/cvc4"|' Data/SBV/Provers/CVC4.hs
          ''
        else "";
      configureFlags = attrs.configureFlags ++ [
        "--ghc-option=-O2"
      ];
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
      self-hs.callCabal2nix "hevm" (./src/hevm) {
        # Haskell libs with the same names as C libs...
        # Depend on the C libs, not the Haskell libs.
        # These are system deps, not Cabal deps.
        inherit (pkgs) secp256k1;

        ff = pkgs.libff;
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
      (if stdenv.isDarwin then [] else [
          "--enable-executable-static"
          "--extra-lib-dirs=${pkgs.gmp.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.glibc.static}/lib"
          "--extra-lib-dirs=${pkgs.libff}/lib"
          "--extra-lib-dirs=${pkgs.ncurses.override {enableStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      ]);
    }));
  }
