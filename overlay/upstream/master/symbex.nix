{ mkDerivation, aeson, base, bytestring, mtl, s-cargot, stdenv
, text, uniplate
}:
mkDerivation {
  pname = "symbex";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring mtl s-cargot text uniplate
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/dapphub/symbex";
  description = "Ethereum symbolic execution engine";
  license = stdenv.lib.licenses.agpl3;
}
