{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, base64-bytestring, binary, bytestring, containers, criterion
, cryptonite, data-dword, deepseq, directory, filepath, ghci-pretty
, here, HUnit, lens, lens-aeson, memory, mtl, optparse-generic
, process, QuickCheck, readline, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, temporary, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "hsevm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring base64-bytestring binary bytestring
    containers criterion cryptonite data-dword deepseq directory
    ghci-pretty lens lens-aeson memory mtl optparse-generic process
    readline temporary text unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson attoparsec base base16-bytestring bytestring containers
    data-dword directory filepath ghci-pretty lens lens-aeson mtl
    optparse-generic readline text unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring ghci-pretty here HUnit lens mtl QuickCheck tasty
    tasty-hunit tasty-quickcheck tasty-smallcheck text vector
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  homepage = "https://github.com/mbrock/hsevm";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
