{ mkDerivation, aeson, base, base16-bytestring, bytestring, mtl
, s-cargot, stdenv, text, uniplate
}:
mkDerivation {
  pname = "symbex";
  version = "0.6.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring mtl s-cargot text uniplate
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/dapphub/symbex";
  description = "Ethereum symbolic execution engine";
  license = stdenv.lib.licenses.agpl3;
}
