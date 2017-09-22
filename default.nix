{ mkDerivation, abstract-par, aeson, ansi-wl-pprint, async, base
, base16-bytestring, base64-bytestring, binary, brick, bytestring
, cereal, containers, cryptonite, data-dword, deepseq, directory
, filepath, ghci-pretty, here, HUnit, lens, lens-aeson, memory
, monad-par, mtl, optparse-generic, process, QuickCheck
, quickcheck-text, readline, restless-git, rosezipper, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, text-format, time, unordered-containers, vector, vty
}:
mkDerivation {
  pname = "hevm";
  version = "0.8";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    abstract-par aeson ansi-wl-pprint base base16-bytestring
    base64-bytestring binary brick bytestring cereal containers
    cryptonite data-dword deepseq directory filepath ghci-pretty lens
    lens-aeson memory monad-par mtl optparse-generic process QuickCheck
    quickcheck-text readline restless-git rosezipper scientific
    temporary text text-format time unordered-containers vector vty
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint async base base16-bytestring base64-bytestring
    binary brick bytestring containers cryptonite data-dword deepseq
    directory filepath ghci-pretty lens lens-aeson memory mtl
    optparse-generic process QuickCheck quickcheck-text readline
    temporary text text-format unordered-containers vector vty
  ];
  testHaskellDepends = [
    base binary bytestring ghci-pretty here HUnit lens mtl QuickCheck
    tasty tasty-hunit tasty-quickcheck text vector
  ];
  homepage = "https://github.com/mbrock/hevm";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
