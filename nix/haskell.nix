# This file was extracted from overlay.nix in order to provide access
# to our Haskell package extensions from other overlays, bypassing the
# rest of our overlay.  This was necessary for rather obscure reasons.

{ pkgs, lib }:

let
  stdenv = pkgs.stdenv;
  "megaparsec_6_2_0" =
    ({ mkDerivation, base, bytestring, case-insensitive, containers
     , criterion, deepseq, hspec, hspec-expectations, mtl
     , parser-combinators, QuickCheck, scientific, text, transformers
     , weigh
     }:
     mkDerivation {
       pname = "megaparsec";
       version = "6.2.0";
       sha256 = "1kyn7fcyckbjngpyxd2d4mny95sy71rk2s22yrkwyjgkza0fvslg";
       libraryHaskellDepends = [
         base bytestring case-insensitive containers deepseq mtl
         parser-combinators scientific text transformers
       ];
       testHaskellDepends = [
         base bytestring containers hspec hspec-expectations mtl QuickCheck
         scientific text transformers
       ];
       benchmarkHaskellDepends = [ base criterion deepseq text weigh ];
       homepage = "https://github.com/mrkkrp/megaparsec";
       description = "Monadic parser combinators";
       license = stdenv.lib.licenses.bsd2;
       hydraPlatforms = stdenv.lib.platforms.none;
     });

in self-hs: super-hs:
  let
    dontCheck = x:
      pkgs.haskell.lib.dontCheck
        (self-hs.callPackage x {});
    hevmBinPath = lib.makeBinPath (with pkgs; [bash coreutils git]);
  in {
    restless-git = dontCheck (import ../restless-git);
    symbex = dontCheck (import ../symbex);
    ethjet = self-hs.callPackage (import ../libethjet-haskell) {
      # Haskell libs with the same names as C libs...
      # Depend on the C libs, not the Haskell libs.
      # These are system deps, not Cabal deps.
      inherit (pkgs) secp256k1 ethjet;
    };

    # We don't want Megaparsec 5!
    megaparsec = self-hs.callPackage megaparsec_6_2_0 {};

    wreq = pkgs.haskell.lib.doJailbreak super-hs.wreq;

    hevm = pkgs.haskell.lib.dontHaddock ((
      self-hs.callPackage (import ../hevm) {}
    ).overrideAttrs (attrs: {
      postInstall = ''
        wrapProgram $out/bin/hevm \
           --suffix PATH : "${lib.makeBinPath (with pkgs; [bash coreutils git])}"
      '';

      enableSeparateDataOutput = true;
      buildInputs = attrs.buildInputs ++ [pkgs.solc];
      nativeBuildInputs = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
    }));
  }
