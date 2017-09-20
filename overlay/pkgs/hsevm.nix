{ mkDerivation, abstract-par, aeson, ansi-wl-pprint, async, base
, base16-bytestring, base64-bytestring, binary, brick, bytestring
, cereal, containers, cryptonite, data-dword, deepseq, directory
, filepath, ghci-pretty, here, HUnit, lens
, lens-aeson, memory, monad-par, mtl, optparse-generic, process
, QuickCheck, quickcheck-text, readline, rosezipper, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, text-format, time, unordered-containers, vector, vty

, restless-git

, fetchFromGitHub, lib, makeWrapper
, ncurses, zlib, bzip2, solc, coreutils
, bash
}:

lib.overrideDerivation (mkDerivation rec {
  pname = "hsevm";
  version = "unstable-2017-09-20";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "hsevm";
    rev = "dda5a0b1cb07fa32e780977d68aa8667720b3172";
    sha256 = "1rla1ka5sg426vy7rxc6r7hskipfgp3j6s0g9ypbz375xg24rcfr";
  };

  postInstall = ''
    wrapProgram $out/bin/hsevm \
       --add-flags '+RTS -N$((`${coreutils}/bin/nproc` - 1)) -RTS' \
       --suffix PATH : "${lib.makeBinPath [bash coreutils]}"
  '';

  enableSeparateDataOutput = true;

  extraLibraries = [
    abstract-par aeson ansi-wl-pprint base base16-bytestring
    base64-bytestring binary brick bytestring cereal containers
    cryptonite data-dword deepseq directory filepath ghci-pretty lens
    lens-aeson memory monad-par mtl optparse-generic process QuickCheck
    quickcheck-text readline rosezipper scientific temporary text text-format
    unordered-containers vector vty restless-git
  ];
  executableHaskellDepends = [
    async readline zlib bzip2
  ];
  testHaskellDepends = [
    base binary bytestring ghci-pretty here HUnit lens mtl QuickCheck
    tasty tasty-hunit tasty-quickcheck text vector
  ];

  homepage = https://github.com/dapphub/hsevm;
  description = "Ethereum virtual machine evaluator";
  license = stdenv.lib.licenses.agpl3;
  maintainers = [stdenv.lib.maintainers.dbrock];
}) (attrs: {
  buildInputs = attrs.buildInputs ++ [solc];
  nativeBuildInputs = attrs.nativeBuildInputs ++ [makeWrapper];
})
