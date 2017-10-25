{ mkDerivation, aeson, base, bytestring, containers, HUnit, stdenv
, tasty, tasty-hunit, text, unix, unordered-containers, vector
, fetchFromGitHub, lib
}:

mkDerivation {
  pname = "jays";
  version = "1.20171020";
  src = fetchFromGitHub {
    owner = "mbrock";
    repo = "jays";
    rev = "v1.20171020";
    sha256 = "1d83zvv55gm417pxzzbfl2s2f80mia0fxidqs0lahfppb6zb09fp";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers text unordered-containers vector
  ];
  executableHaskellDepends = [ base bytestring text unix ];
  testHaskellDepends = [
    aeson base bytestring HUnit tasty tasty-hunit text
  ];

  postInstall = ''
    cp $out/bin/{jays,jshon}
  '';

  maintainers = [lib.maintainers.dbrock];
  homepage = "https://github.com/mbrock/jays";
  description = "Rewrite of jshon";
  license = stdenv.lib.licenses.gpl3;
}
