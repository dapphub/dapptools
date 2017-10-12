{ mkDerivation, aeson, base, bytestring, containers, HUnit, stdenv
, tasty, tasty-hunit, text, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "jays";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers text unordered-containers vector
  ];
  executableHaskellDepends = [ base bytestring text unix ];
  testHaskellDepends = [
    aeson base bytestring HUnit tasty tasty-hunit text
  ];
  homepage = "https://github.com/mbrock/jays";
  description = "Rewrite of jshon";
  license = stdenv.lib.licenses.gpl3;
}
