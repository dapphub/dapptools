{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales
, bc, coreutils, curl, ethabi, ethsign, git, gnused, jshon, nodejs, perl, solc }:

stdenv.mkDerivation rec {
  name = "seth-${version}";
  version = "0.8.2";
  src = ./.;

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    bc coreutils curl ethabi ethsign git gnused jshon nodejs perl solc
  ]; in ''
    wrapProgram "$out/bin/seth" --prefix PATH : "${path}" \
      ${if glibcLocales != null then
        "--set LOCALE_ARCHIVE \"${glibcLocales}\"/lib/locale/locale-archive"
        else ""}
  '';

  meta = {
    description = "Command-line client for talking to Ethereum nodes";
    homepage = https://github.com/dapphub/dapptools/src/seth/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
