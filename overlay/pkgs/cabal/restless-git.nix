{ mkDerivation, base, bytestring, containers, HSH, stdenv, tasty
, tasty-hunit, temporary, text, time
}:
mkDerivation {
  pname = "restless-git";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers HSH text time
  ];
  testHaskellDepends = [
    base bytestring containers tasty tasty-hunit temporary text
  ];
  homepage = "https://github.com/lessrest/restless-git";
  description = "Easy Git repository serialization";
  license = stdenv.lib.licenses.gpl3;
}
