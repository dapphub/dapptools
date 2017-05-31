{ mkDerivation, aeson, ansi-wl-pprint, base, base16-bytestring
, base64-bytestring, binary, brick, bytestring, containers
, cryptonite, data-dword, deepseq, directory, filepath, ghci-pretty
, here, HUnit, lens, lens-aeson, memory, mtl, optparse-generic
, process, QuickCheck, quickcheck-text, readline, rosezipper
, stdenv, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, text-format, unordered-containers, vector, vty
}:
mkDerivation {
  pname = "hsevm";
  version = "0.1.0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring base64-bytestring
    binary brick bytestring containers cryptonite data-dword deepseq
    directory filepath ghci-pretty lens lens-aeson memory mtl
    optparse-generic process QuickCheck quickcheck-text readline
    rosezipper temporary text text-format unordered-containers vector
    vty
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring base64-bytestring
    binary brick bytestring containers cryptonite data-dword deepseq
    directory filepath ghci-pretty lens lens-aeson memory mtl
    optparse-generic process QuickCheck quickcheck-text readline
    temporary text text-format unordered-containers vector vty
  ];
  testHaskellDepends = [
    base binary bytestring ghci-pretty here HUnit lens mtl QuickCheck
    tasty tasty-hunit tasty-quickcheck text vector
  ];
  homepage = "https://github.com/mbrock/hsevm";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
