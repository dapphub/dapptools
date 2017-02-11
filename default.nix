{ mkDerivation, aeson, attoparsec, base, binary, bytestring
, containers, criterion, cryptonite, data-dword, deepseq, directory
, filepath, ghci-pretty, HUnit, lens, lens-aeson, memory, process
, QuickCheck, readline, stdenv, tasty, tasty-hunit
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
    aeson base binary bytestring containers criterion cryptonite
    data-dword deepseq directory ghci-pretty lens lens-aeson memory
    process temporary text unordered-containers vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers data-dword directory filepath
    ghci-pretty lens readline text vector
  ];
  testHaskellDepends = [
    base ghci-pretty HUnit QuickCheck tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  homepage = "https://github.com/mbrock/hsevm";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
