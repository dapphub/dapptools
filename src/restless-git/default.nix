{ mkDerivation, base, bytestring, clock, containers, HSH, stdenv
, tasty, tasty-hunit, temporary, text, time
}:
mkDerivation {
  pname = "restless-git";
  version = "0.7";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring clock containers HSH text time
  ];
  testHaskellDepends = [
    base bytestring containers tasty tasty-hunit temporary text
  ];
  homepage = "https://github.com/dapphub/dapptools";
  description = "Easy Git repository serialization";
  license = stdenv.lib.licenses.gpl3;
}
