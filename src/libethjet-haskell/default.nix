{ mkDerivation, base, base16-bytestring, bytestring, ethjet, HUnit
, secp256k1, stdenv, tasty, tasty-hunit
}:
mkDerivation {
  pname = "ethjet";
  version = "0.5";
  src = ./.;
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ ethjet secp256k1 ];
  testHaskellDepends = [
    base base16-bytestring bytestring HUnit tasty tasty-hunit
  ];
  homepage = "https://github.com/dapphub/libethjet";
  description = "Binding to libethjet for Ethereum precompiled contracts";
  license = stdenv.lib.licenses.gpl3;
}
