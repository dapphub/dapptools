{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales
, coreutils, git, gnused, gnumake, hevm, jshon, jq, nix
, nodejs, perl, seth, shellcheck, solc, dapptoolsSrc }:

stdenv.mkDerivation rec {
  name = "dapp-${version}";
  version = "0.31.1";
  src = ./.;

  nativeBuildInputs = [makeWrapper shellcheck coreutils nodejs];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];

  postInstall =
    let
      path = lib.makeBinPath [
        coreutils git gnused gnumake hevm jshon jq nix nodejs perl seth solc
      ];
    in
      ''
        wrapProgram "$out/bin/dapp" \
          --prefix PATH : ${path} \
          --set DAPPTOOLS ${dapptoolsSrc} \
        ${lib.optionalString (glibcLocales != null) ''
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
      ''}
  '';

  meta = {
    description = "Simple tool for creating Ethereum-based dapps";
    homepage = https://github.com/dapphub/dapptools/src/dapp/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
