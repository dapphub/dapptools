{

# Nix dependencies
  mkDerivation
, lib
, stdenv
, fetchFromGitHub
, makeWrapper

# Haskell dependencies

, HUnit
, QuickCheck
, abstract-par
, aeson
, ansi-wl-pprint
, async
, base
, base16-bytestring
, base64-bytestring
, binary
, brick_0_24_2
, bytestring
, cereal
, containers
, cryptonite
, data-dword
, deepseq
, directory
, filepath
, ghci-pretty
, here
, lens
, lens-aeson
, memory
, monad-par
, mtl
, operational
, optparse-generic
, process
, quickcheck-text
, readline
, restless-git
, rosezipper
, scientific
, tasty
, tasty-hunit
, tasty-quickcheck
, temporary
, text
, text-format
, time
, unordered-containers
, vector
, vty
, wreq

# Program dependencies

, bash
, bzip2
, coreutils
, ncurses
, solc
, zlib

}:

lib.overrideDerivation (mkDerivation rec {
  pname = "hevm";
  version = "0.9";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "hevm";
    rev = "v0.9";
    sha256 = "15s4qal7i56m9i30i5n9y5wmzv01q1mzxzf4m4k22vmvmcdnc3s3";
  };

  postInstall = ''
    wrapProgram $out/bin/hevm \
       --add-flags '+RTS -N$((`${coreutils}/bin/nproc` - 1)) -RTS' \
       --suffix PATH : "${lib.makeBinPath [bash coreutils]}"
  '';

  enableSeparateDataOutput = true;

  extraLibraries = [
    abstract-par aeson ansi-wl-pprint base base16-bytestring
    base64-bytestring binary brick_0_24_2 bytestring cereal containers
    cryptonite data-dword deepseq directory filepath ghci-pretty lens
    lens-aeson memory monad-par mtl optparse-generic operational process QuickCheck
    quickcheck-text readline rosezipper scientific temporary text text-format
    unordered-containers vector vty restless-git wreq
  ];
  executableHaskellDepends = [
    async readline zlib bzip2
  ];
  testHaskellDepends = [
    base binary bytestring ghci-pretty here HUnit lens mtl QuickCheck
    tasty tasty-hunit tasty-quickcheck text vector
  ];

  homepage = https://github.com/dapphub/hevm;
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
  maintainers = [stdenv.lib.maintainers.dbrock];
}) (attrs: {
  buildInputs = attrs.buildInputs ++ [solc];
  nativeBuildInputs = attrs.nativeBuildInputs ++ [makeWrapper];
})
