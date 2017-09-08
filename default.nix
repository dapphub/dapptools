{ mkDerivation, base, bytestring, containers, gitlib
, gitlib-cmdline, hedgehog, shelly, stdenv, tasty, tasty-hedgehog
, temporary, text, time
}:
mkDerivation {
  pname = "restless-git";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers gitlib gitlib-cmdline shelly text time
  ];
  testHaskellDepends = [
    base bytestring containers hedgehog tasty tasty-hedgehog temporary
    text
  ];
  homepage = "https://github.com/lessrest/restless-git";
  description = "Easy Git repository serialization";
  license = stdenv.lib.licenses.gpl3;
}
