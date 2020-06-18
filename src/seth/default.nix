{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales, jq
, bc, coreutils, curl, ethsign, git, gnused, jshon, nodejs, perl, hevm, shellcheck }:

stdenv.mkDerivation rec {
  name = "seth-${version}";
  version = "0.9.0";
  src = ./.;

  nativeBuildInputs = [makeWrapper shellcheck];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];

  postInstall =
    let
      path = lib.makeBinPath [
        bc coreutils curl ethsign git gnused hevm jshon nodejs perl jq
      ];
    in
      ''
        wrapProgram "$out/bin/seth" \
          --prefix PATH : ${path} \
        ${lib.optionalString (glibcLocales != null) ''
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
      ''}
  '';

  meta = {
    description = "Command-line client for talking to Ethereum nodes";
    homepage = https://github.com/dapphub/dapptools/src/seth/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
