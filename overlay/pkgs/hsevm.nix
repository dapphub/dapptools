{ mkDerivation, abstract-par, aeson, ansi-wl-pprint, base
, base16-bytestring, base64-bytestring, binary, brick, bytestring
, cereal, containers, cryptonite, data-dword, deepseq, directory
, filepath, ghci-pretty, gitlib, gitlib-libgit2, here, HUnit, lens
, lens-aeson, memory, monad-par, mtl, optparse-generic, process
, QuickCheck, quickcheck-text, readline, rosezipper, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, text-format, time, unordered-containers, vector, vty

, fetchFromGitHub, lib, makeWrapper
, ncurses, zlib, bzip2, solc, coreutils
}:

lib.overrideDerivation (mkDerivation rec {
  pname = "hsevm";
  version = "0.7";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "hsevm";
    rev = "v${version}";
    sha256 = "0grsp2dziv6pvwx8y5i2m3079lwc67kccppm0szg5y69g3i2pzy7";
  };

  postInstall = ''
    wrapProgram $out/bin/hsevm --add-flags '+RTS -N$((`${coreutils}/bin/nproc` - 1)) -RTS'
  '';

  extraLibraries = [
    abstract-par aeson ansi-wl-pprint base base16-bytestring
    base64-bytestring binary brick bytestring cereal containers
    cryptonite data-dword deepseq directory filepath ghci-pretty lens
    lens-aeson memory monad-par mtl optparse-generic process QuickCheck
    quickcheck-text readline rosezipper scientific temporary text text-format
    unordered-containers vector vty gitlib gitlib-libgit2
  ];
  executableHaskellDepends = [
    readline zlib bzip2
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
