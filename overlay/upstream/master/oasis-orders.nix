{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, containers, data-dword, directory, hevm, lens, lens-aeson, pipes
, pipes-text, stdenv, text, vector
}:
mkDerivation {
  pname = "oasis-orders";
  version = "0.6";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16-bytestring binary bytestring containers
    data-dword directory hevm lens lens-aeson pipes pipes-text text
    vector
  ];
  homepage = "https://github.com/dapphub/libethjet";
  description = "Parse order books from OasisDEX JSON logs";
  license = stdenv.lib.licenses.gpl3;
}
