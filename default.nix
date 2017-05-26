{ mkDerivation, aeson, ansi-wl-pprint, base, base16-bytestring
, base64-bytestring, binary, brick, bytestring, containers
, cryptonite, data-dword, deepseq, directory, filepath, ghci-pretty
, here, HUnit, lens, lens-aeson, memory, mtl, optparse-generic
, process, QuickCheck, quickcheck-text, readline, stdenv, tasty
, tasty-hunit, tasty-quickcheck, temporary, text, text-format
, unordered-containers, vector, vty
}:
mkDerivation {
  pname = "hsevm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring base64-bytestring
    binary brick bytestring containers cryptonite data-dword deepseq
    directory ghci-pretty lens lens-aeson memory mtl optparse-generic
    process QuickCheck quickcheck-text readline temporary text
    text-format unordered-containers vector vty
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring containers data-dword
    directory filepath ghci-pretty lens lens-aeson mtl optparse-generic
    readline text unordered-containers vector
  ];
  testHaskellDepends = [
    base binary bytestring ghci-pretty here HUnit lens mtl QuickCheck
    tasty tasty-hunit tasty-quickcheck text vector
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  homepage = "https://github.com/mbrock/hsevm";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
