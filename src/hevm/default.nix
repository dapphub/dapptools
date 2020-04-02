{ mkDerivation, abstract-par, aeson, ansi-wl-pprint, async, base
, base16-bytestring, base64-bytestring, binary, brick, bytestring
, cborg, cereal, containers, cryptonite, data-dword, deepseq
, directory, ff, fgl, filepath, free, ghci-pretty, haskeline, here
, HUnit, lens, lens-aeson, megaparsec, memory, monad-par, mtl
, multiset, operational, optparse-generic, process, QuickCheck
, quickcheck-text, regex-tdfa, restless-git, rosezipper, s-cargot
, sbv_8_4, scientific, secp256k1, semver-range, stdenv, tasty
, tasty-hunit, tasty-quickcheck, temporary, text, text-format, time
, transformers, tree-view, unordered-containers, vector, vty
, witherable, wreq
}:
mkDerivation {
  pname = "hevm";
  version = "0.38";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    abstract-par aeson ansi-wl-pprint base base16-bytestring
    base64-bytestring binary brick bytestring cborg cereal containers
    cryptonite data-dword deepseq directory fgl filepath free
    ghci-pretty haskeline lens lens-aeson megaparsec memory monad-par
    mtl multiset operational optparse-generic process QuickCheck
    quickcheck-text restless-git rosezipper s-cargot sbv_8_4 scientific
    semver-range temporary text text-format time transformers tree-view
    unordered-containers vector vty witherable wreq
  ];
  librarySystemDepends = [ ff secp256k1 ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint async base base16-bytestring base64-bytestring
    binary brick bytestring containers cryptonite data-dword deepseq
    directory filepath free ghci-pretty lens lens-aeson memory mtl
    optparse-generic process QuickCheck quickcheck-text regex-tdfa sbv_8_4
    temporary text text-format unordered-containers vector vty
  ];
  testHaskellDepends = [
    base base16-bytestring binary bytestring free ghci-pretty here
    HUnit lens mtl QuickCheck sbv_8_4 tasty tasty-hunit tasty-quickcheck
    text vector
  ];
  testSystemDepends = [ secp256k1 ];
  homepage = "https://github.com/dapphub/dapptools";
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
}
